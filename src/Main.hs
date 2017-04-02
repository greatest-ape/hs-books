{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Graphics.GD as GD
import qualified Network.CGI as CGI
import qualified Text.HTML.TagSoup as TagSoup

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)
import Crypto.Hash (Digest, SHA256, digestToHexByteString, hashlazy, hash)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isSuffixOf, isInfixOf, nub, scanl', words, unwords)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist)



-- * Settings


bookDirectory           = "media/books"
thumbnailDirectory      = "media/covers/small"

jpegQuality             = 80

filenameCachePath       = ".filename-cache"
jsonCachePath           = ".json-cache"

imageMaxWidth  = 16 * 15
imageMaxHeight = 16 * 15



-- * Types and instances


-- Identifies a cover image
type Identifier = String


data Cover = Cover {
    _thumbnailPath :: String
} deriving (Generic, Show)


data Book = Book {
    _path       :: Text.Text,
    _maybeCover :: Maybe Cover,
    _titles     :: [String],
    _creators   :: [String],
    _dates      :: [String],
    _publishers :: [String],
    _languages  :: [String],
    _textBytes  :: Integer
} deriving (Generic, Show)


instance JSON.ToJSON Cover where
    toEncoding = JSON.genericToEncoding JSON.defaultOptions

instance JSON.ToJSON Book where
    toEncoding = JSON.genericToEncoding JSON.defaultOptions



-- * Start of main program


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = CGI.runCGI $ CGI.handleErrors $ do
    filenames <- filter (".epub" `isSuffixOf`) <$> (CGI.liftIO $ listDirectory bookDirectory)

    -- Check cache, generate JSON anew if necessary

    let filenameHash = digestToHexByteString $ (hash $ Char8.pack $ show filenames :: Digest SHA256)

    forceReload <- CGI.getInput "force-reload"

    json <- case forceReload of
        Just _  -> generateJsonAndSaveState filenames  filenameHash
        Nothing -> do
            eitherJsonCache <- liftIO $ readJsonCache filenameHash

            -- Either use cache JSON or generate it anew
            case eitherJsonCache of
                Left _          -> generateJsonAndSaveState filenames filenameHash
                Right jsonCache -> return $ LBS.fromStrict jsonCache

    -- CGI output

    CGI.setHeader "Content-type" "application/json"
    CGI.outputFPS json

    where
        -- Attempt to retrieve JSON from the cache
        -- Fails when files can't be or and when the filename hashes don't match
        readJsonCache :: BS.ByteString -> IO (Either SomeException BS.ByteString)
        readJsonCache filenameHash = try $ do
            filenameCache <- BS.readFile filenameCachePath
            jsonCache     <- BS.readFile jsonCachePath

            if filenameCache == filenameHash
                then return jsonCache
                else error "Filenames not matching hashed filenames"

        -- Use rest of program to create books and generate JSON from filenames
        -- Save the results in cache
        generateJsonAndSaveState filenames filenameHash = do
            json <- JSON.encode . rights <$> (CGI.liftIO $ mapM readBook filenames)

            liftIO $ BS.writeFile filenameCachePath filenameHash
            liftIO $ LBS.writeFile jsonCachePath json

            return json


-- Attempt to create a Book from a file path to an epub file
readBook :: String -> IO (Either String Book)
readBook archiveFilename = runErrorT $ do
    let path       = bookDirectory ++ "/" ++ archiveFilename
        hash       = hashlazy $ fromString archiveFilename :: Digest SHA256
        identifier = Char8.unpack $ digestToHexByteString hash

    xmlString       <- Epub.getPkgXmlFromZip path
    manifest        <- Epub.getManifest xmlString
    package         <- Epub.getPackage xmlString
    metadata        <- Epub.getMetadata xmlString

    textBytes <- liftIO $ getNumberOfTextBytes path

    eitherMaybeCoverImage <- liftIO $ getCoverImage path manifest identifier
    let maybeCoverImage = case eitherMaybeCoverImage of
            Right maybeCoverImage -> maybeCoverImage
            Left _                -> Nothing

    let titles      = map Epub.titleText $ Epub.metaTitles metadata
        creators    = map extractNameWithComma $ Epub.metaCreators metadata
        dates       = map (\(Epub.Date _ text) -> text) $ Epub.metaDates metadata
        publishers  = Epub.metaPublishers metadata
        languages   = Epub.metaLangs metadata

    return $ Book {
        _path       = Text.decodeUtf8 $ Char8.pack path,
        _maybeCover = maybeCoverImage,
        _titles     = titles,
        _creators   = creators,
        _dates      = dates,
        _publishers = publishers,
        _languages  = languages,
        _textBytes  = textBytes
    }


-- Take a Creator and extract a name in the format "[LAST NAME], [FIRST NAME]"
extractNameWithComma :: Epub.Creator -> String
extractNameWithComma creator =
    let name = case Epub.creatorFileAs creator of
            Just fileAs -> fileAs
            Nothing     -> Epub.creatorText creator
    in case length $ splitOn "," name of
        1 -> buildNameWithComma name
        2 -> name
        _ -> Epub.creatorText creator -- Absolute edge case

    where
        -- Format a name without a comma "LAST NAME, FIRST NAME(S)"
        buildNameWithComma name =
            let parts = map (filter (/= ',')) $ words name
            in if length parts == 1
                then last parts
                else last parts ++ ", " ++ unwords (init parts)


-- Get the number of bytes the text content files in an epub archive take up
getNumberOfTextBytes :: FilePath -> IO Integer
getNumberOfTextBytes archivePath = do
    allEntries <- Zip.zEntries . Zip.toArchive <$> LBS.readFile archivePath

    return $ sum $ map
        (getLengthWithoutMarkup . Zip.eCompressedData)
        (filter entryIsTextContentFile allEntries)

    where
        entryIsTextContentFile entry =
            let relativePath = map toLower $ Zip.eRelativePath entry
            in any (\ending -> ending `isSuffixOf` relativePath)
                [".html", ".htm", ".xhtml", ".xml"]

        getLengthWithoutMarkup :: LBS.ByteString -> Integer
        getLengthWithoutMarkup s =
            let tagTexts = filter TagSoup.isTagText $ TagSoup.parseTags s
            in fromIntegral $ sum $ map (LBS.length . TagSoup.fromTagText) tagTexts



-- * Cover images


-- Given an archive path, a epub manifest and a string identifier try to find
-- a cover image
-- If found, save a thumbnail and return a Cover
getCoverImage
    :: FilePath
    -> Epub.Manifest
    -> Identifier
    -> IO (Either SomeException (Maybe Cover))
getCoverImage archivePath manifest identifier = try $ do
    let thumbnailPath  = thumbnailDirectory ++ "/" ++ identifier ++ ".jpg"
        justCover      = Just $ Cover thumbnailPath

    fileExists <- doesFileExist thumbnailPath

    if fileExists
        then return justCover
        else do
            imageData <- getImageData archivePath manifest

            case imageData of
                Just (mediaType, imageByteString) -> do
                    success <- saveThumbnail thumbnailPath mediaType imageByteString

                    case success of
                        True  -> return justCover
                        False -> return Nothing

                _ -> return Nothing


-- Given an archive path and a manifest, attempt to find a cover image and
-- return its data as a ByteString
getImageData
    :: FilePath
    -> Epub.Manifest
    -> IO (Maybe (String, LBS.ByteString))
getImageData archivePath manifest = do
    archive <- Zip.toArchive <$> LBS.readFile archivePath

    return $ do
        manifestItem <- getCoverManifestItem manifest
        maybeByteString <- findFileInArchive archive $ Epub.mfiHref manifestItem
        Just (Epub.mfiMediaType manifestItem, maybeByteString)


-- Given an epub manifest, attempts to find a ManifestItem for a cover image
getCoverManifestItem :: Epub.Manifest -> Maybe Epub.ManifestItem
getCoverManifestItem (Epub.Manifest items) =
    maybeHead $ filter (\i -> isImage i && (hasCoverInHref i || hasCoverInID i)) items

    where
        isImage manifestItem        = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        hasCoverInHref manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)
        hasCoverInID manifestItem   = "cover" `isInfixOf` map toLower (Epub.mfiId manifestItem)


-- Extract a file in a zip archive by the file path.
-- Looks for the file in all folders in the archive.
findFileInArchive :: Zip.Archive -> FilePath -> Maybe LBS.ByteString
findFileInArchive archive path =
    let folders = extractFolders $ Zip.filesInArchive archive
        paths   = path : map (\folder -> folder ++ path) folders
        entries = catMaybes $ map (flip Zip.findEntryByPath archive) paths
    in Zip.fromEntry <$> maybeHead entries


-- Take a list of paths, extract all folders
extractFolders :: [FilePath] -> [FilePath]
extractFolders paths = "" : (nub $ paths >>= f) -- The empty string adds the root dir
    where
        f path =
            let parts       = splitOn "/" path
                l           = length parts
                folderParts = take (l - 1) parts
            in
                if l > 1
                    then scanl' (\a b -> a ++ b ++ "/") "" folderParts
                    else []


-- Save a thumbnail of an image with GD
saveThumbnail
    :: FilePath
    -> String
    -> LBS.ByteString
    -> IO Bool
saveThumbnail thumbnailPath mediaType imageByteString = do
    image <- getImageLoader mediaType $ LBS.toStrict imageByteString

    -- Calculate thumbnail dimensions, create and save thumbnail
    (width, height) <- GD.imageSize image
    let (newWidth, newHeight) = calculateThumbnailSize width height
    thumbnail <- GD.resizeImage newWidth newHeight image
    GD.saveJpegFile jpegQuality thumbnailPath thumbnail

    return True

    where
        getImageLoader :: String -> (BS.ByteString -> IO GD.Image)
        getImageLoader "image/png"  = GD.loadPngByteString
        getImageLoader "image/jpeg" = GD.loadJpegByteString
        getImageLoader "image/gif"  = GD.loadGifByteString


-- On the basis of given dimensions and settings (max sizes), calculate a
-- thumbnail size
calculateThumbnailSize :: Int -> Int -> (Int, Int)
calculateThumbnailSize width height =
    case compare width height of
        EQ -> (imageMaxWidth, imageMaxHeight)
        LT -> swap $ scale height width imageMaxHeight
        GT -> scale width height imageMaxWidth

    where
        scale :: Int -> Int -> Int -> (Int, Int)
        scale a b maxA =
            let newA = fromIntegral maxA
                newB = fromIntegral b * (newA / fromIntegral a)
            in (round newA, round newB)



-- * Utils


maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing
