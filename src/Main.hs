{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Codec.Picture as JuicyPixels
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Network.CGI as CGI
import qualified Vision.Image as Friday
import qualified Vision.Primitive.Shape as Friday

import Control.Exception (IOException, try)
import Control.Monad.Error (runErrorT, liftIO)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isInfixOf, nub, scanl', words, unwords)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist)
import Vision.Image.JuicyPixels (toFridayRGBA, toJuicyRGBA)


-- * Types and instances


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
jpegQuality    = 80



-- * Start of main program


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = CGI.runCGI $ CGI.handleErrors $ do
    filenames <- CGI.liftIO $ listDirectory bookDirectory
    books     <- rights <$> (CGI.liftIO $ mapM readBook filenames)

    CGI.setHeader "Content-type" "application/json\n"
    CGI.outputFPS $ JSON.encode books


-- Attempt to create a Book from a file path to an epub file
readBook :: String -> IO (Either String Book)
readBook archiveFilename = runErrorT $ do
    let path       = bookDirectory ++ "/" ++ archiveFilename
    let identifier = archiveFilename

    xmlString       <- Epub.getPkgXmlFromZip path
    manifest        <- Epub.getManifest xmlString
    package         <- Epub.getPackage xmlString
    metadata        <- Epub.getMetadata xmlString

    maybeCoverImage <- liftIO $ getCoverImage path manifest identifier

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
            in last parts ++ ", " ++ unwords (init parts)


-- * Cover images


-- Given an archive path, a epub manifest and a string identifier try to find
-- a cover image
-- If found, save it in two formats and return a Cover
getCoverImage :: FilePath -> Epub.Manifest -> String -> IO (Maybe Cover)
getCoverImage archivePath manifest identifier = do
    let fullsizePath   = fullsizeImageDirectory ++ "/" ++ identifier ++ ".jpg"
        thumbnailPath  = thumbnailDirectory ++ "/" ++ identifier ++ ".png"
        justCover      = Just $ Cover fullsizePath thumbnailPath

    fileExists <- doesFileExist thumbnailPath

    if fileExists
        then return justCover
        else do
            imageData <- getImageData archivePath manifest

            case imageData of
                Just (mediaType, imageByteString) -> do
                    success <- saveImages fullsizePath thumbnailPath mediaType imageByteString
                    return $ if success then justCover else Nothing
                Nothing -> return Nothing

-- Given an archive path and a manifest, attempt to find a cover image and
-- return its data as a mediaType string and a file data bytestring
getImageData :: FilePath -> Epub.Manifest -> IO (Maybe (String, LBS.ByteString))
getImageData archivePath manifest = do
    archive <- Zip.toArchive <$> LBS.readFile archivePath

    return $ do
        manifestItem <- getCoverManifestItem manifest
        imageBS      <- findFileInArchive archive $ Epub.mfiHref manifestItem
        Just (Epub.mfiMediaType manifestItem, imageBS)


-- Given an epub manifest, attempts to find a cover image
getCoverManifestItem :: Epub.Manifest -> Maybe Epub.ManifestItem
getCoverManifestItem (Epub.Manifest items) =
    maybeHead $ filter (\i -> isImage i && (hasCoverInHref i || hasCoverInID i)) items

    where
        isImage manifestItem = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        hasCoverInHref manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)
        hasCoverInID manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiId manifestItem)


-- Extract a file in a zip archive by the file path. Looks in all root level
-- folders
findFileInArchive :: Zip.Archive -> FilePath -> Maybe LBS.ByteString
findFileInArchive archive path =
    let folders = nub $ extractFolders $ Zip.filesInArchive archive
        paths   = path : map (\folder -> folder ++ "/" ++ path) folders
        entries = catMaybes $ map (flip Zip.findEntryByPath archive) paths
    in Zip.fromEntry <$> maybeHead entries


-- Take a list of paths, return those who seem to be folders
extractFolders :: [FilePath] -> [FilePath]
extractFolders paths = paths >>= f
    where
        f path =
            let parts       = splitOn "/" path
                l           = length parts
                folderParts = take (l - 1) parts
            in
                if l > 1
                    then scanl' (\a b -> a ++ "/" ++ b) "" folderParts
                    else []


-- Save a fullsize version and a thumbnail of an image
saveImages
    :: FilePath
    -> FilePath
    -> String
    -> LBS.ByteString
    -> IO Bool
saveImages fullsizePath thumbnailPath mediaType imageByteString = do
    case JuicyPixels.decodeImage $ LBS.toStrict imageByteString of
        Left _ -> return False
        Right juicyImage -> do
            let fridayImage = toFridayRGBA $ JuicyPixels.convertRGBA8 juicyImage
                Friday.Z Friday.:. height Friday.:. width = Friday.manifestSize fridayImage
                (newWidth, newHeight) = calculateNewSizes width height
                newSize = Friday.ix2 newHeight newWidth

            JuicyPixels.writePng thumbnailPath $
                toJuicyRGBA $ Friday.resize Friday.TruncateInteger newSize fridayImage

            JuicyPixels.writePng fullsizePath $ JuicyPixels.convertRGBA8 juicyImage

            return True

    where
        calculateNewSizes :: Int -> Int -> (Int, Int)
        calculateNewSizes width height =
            case compare width height of
                EQ -> (imageMaxWidth, imageMaxHeight)
                LT -> swap $ scale height width imageMaxHeight
                GT -> scale width height imageMaxWidth

        scale :: Int -> Int -> Int -> (Int, Int)
        scale a b maxA =
            let newA = fromIntegral maxA
                newB = fromIntegral b * (newA / fromIntegral a)
            in (round newA, round newB)

-- * Utils


maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing
