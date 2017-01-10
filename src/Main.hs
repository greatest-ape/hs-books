{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Codec.Picture as JuicyPixels
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Graphics.GD as GD
import qualified Network.CGI as CGI
import qualified Vision.Image as Friday
import qualified Vision.Primitive.Shape as Friday

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)
import Crypto.Hash (Digest, SHA256, digestToHexByteString, hashlazy)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isSuffixOf, isInfixOf, nub, scanl', words, unwords)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist)
import Vision.Image.JuicyPixels (toFridayRGBA, toJuicyRGBA)


-- * Types and instances


-- Identifies a cover image
type Identifier = String


data Cover = Cover {
    _fullsizePath  :: String,
    _thumbnailPath :: String
} deriving (Generic, Show)


data Book = Book {
    _path       :: FilePath,
    _maybeCover :: Maybe Cover,
    _titles     :: [String],
    _creators   :: [String],
    _dates      :: [String],
    _publishers :: [String]
} deriving (Generic, Show)


instance JSON.ToJSON Cover
instance JSON.ToJSON Book



-- * Settings


bookDirectory           = "media/books"
fullsizeImageDirectory  = "media/covers/full"
thumbnailDirectory      = "media/covers/small"

imageMaxWidth  = 16 * 15 * 2
imageMaxHeight = 16 * 15 * 2



-- * Start of main program


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = CGI.runCGI $ CGI.handleErrors $ do
    filenames <- filter (".epub" `isSuffixOf`) <$> (CGI.liftIO $ listDirectory bookDirectory)
    books     <- rights <$> (CGI.liftIO $ mapM readBook filenames)

    CGI.setHeader "Content-type" "application/json\n"
    CGI.outputFPS $ JSON.encode books


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

    eitherMaybeCoverImage <- liftIO $ getCoverImage path manifest identifier
    let maybeCoverImage = case eitherMaybeCoverImage of
            Right maybeCoverImage -> maybeCoverImage
            Left _                -> Nothing

    let titles      = map Epub.titleText $ Epub.metaTitles metadata
        creators    = map extractNameWithComma $ Epub.metaCreators metadata
        dates       = map (\(Epub.Date _ text) -> text) $ Epub.metaDates metadata
        publishers  = Epub.metaPublishers metadata

    return $ Book {
        _path       = path,
        _maybeCover = maybeCoverImage,
        _titles     = titles,
        _creators   = creators,
        _dates      = dates,
        _publishers = publishers
    }


-- Take a Creator and extract a name in the format "[FIRST NAME] [LAST NAME]"
extractNameWithoutComma :: Epub.Creator -> String
extractNameWithoutComma creator =
    let name = case Epub.creatorFileAs creator of
            Just fileAs -> fileAs
            Nothing     -> Epub.creatorText creator
        parts = splitOn "," name
    in
        if length parts > 1
            then concat (tail parts) ++ " " ++ head parts
            else name


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


-- * Cover images


-- Given an archive path, a epub manifest and a string identifier try to find
-- a cover image
-- If found, save it in two formats and return a Cover
getCoverImage
    :: FilePath
    -> Epub.Manifest
    -> Identifier
    -> IO (Either SomeException (Maybe Cover))
getCoverImage archivePath manifest identifier = try $ do
    let fullsizePath   = fullsizeImageDirectory ++ "/" ++ identifier ++ ".png"
        thumbnailPath  = thumbnailDirectory ++ "/" ++ identifier ++ ".png"
        justCover      = Just $ Cover fullsizePath thumbnailPath

    fileExists <- doesFileExist thumbnailPath

    if fileExists
        then return justCover
        else do
            imageData <- getImageData archivePath manifest

            case imageData of
                Just (mediaType, imageByteString) -> do
                    -- success <- saveImagesFridayJuicy fullsizePath thumbnailPath imageByteString
                    success <- saveImagesGD fullsizePath thumbnailPath mediaType imageByteString

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
        return (Epub.mfiMediaType manifestItem, maybeByteString)


-- Given an epub manifest, attempts to find a ManifestItem for a cover image
getCoverManifestItem :: Epub.Manifest -> Maybe Epub.ManifestItem
getCoverManifestItem (Epub.Manifest items) =
    maybeHead $ filter (\i -> isImage i && (hasCoverInHref i || hasCoverInID i)) items

    where
        isImage manifestItem        = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        hasCoverInHref manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)
        hasCoverInID manifestItem   = "cover" `isInfixOf` map toLower (Epub.mfiId manifestItem)


-- Extract a file in a zip archive by the file path. Looks for the file in all
-- folders in the archive
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


-- Save a fullsize version and a thumbnail of an image with Friday and JuicyPixels
saveImagesFridayJuicy
    :: FilePath
    -> FilePath
    -> LBS.ByteString
    -> IO Bool
saveImagesFridayJuicy fullsizePath thumbnailPath imageByteString = do
    case JuicyPixels.decodeImage $ LBS.toStrict imageByteString of
        Left _ -> return False
        Right juicyImage -> do
            let fridayImage = toFridayRGBA $ JuicyPixels.convertRGBA8 juicyImage
                Friday.Z Friday.:. height Friday.:. width = Friday.manifestSize fridayImage
                (newWidth, newHeight) = calculateThumbnailSize width height
                newSize = Friday.ix2 newHeight newWidth

            liftIO $ JuicyPixels.writePng thumbnailPath $
                toJuicyRGBA $ Friday.resize Friday.TruncateInteger newSize fridayImage

            liftIO $ JuicyPixels.writePng fullsizePath $ JuicyPixels.convertRGBA8 juicyImage

            return True


-- Save a fullsize version and a thumbnail of an image with GD
saveImagesGD
    :: FilePath
    -> FilePath
    -> String
    -> LBS.ByteString
    -> IO Bool
saveImagesGD fullsizePath thumbnailPath mediaType imageByteString = liftIO $ do
    let imageByteStringStrict = LBS.toStrict imageByteString

    image <- case mediaType of
        "image/png" -> GD.loadJpegByteString imageByteStringStrict
        "image/jpeg" -> GD.loadPngByteString imageByteStringStrict
        "image/gif" -> GD.loadGifByteString imageByteStringStrict

    -- Save full-size copy
    GD.savePngFile fullsizePath image

    -- Calculate thumbnail dimensions, create and save thumbnail
    (width, height) <- GD.imageSize image
    let (newWidth, newHeight) = calculateThumbnailSize width height
    thumbnail <- GD.resizeImage newWidth newHeight image
    GD.savePngFile thumbnailPath thumbnail

    return True


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
