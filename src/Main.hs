{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy.Char8 as Char8

import Control.Monad (forM_)
import Control.Monad.Error (runErrorT, liftIO)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isInfixOf, nub, scanl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.Directory (listDirectory)


-- * Types


data Cover = Cover {
    _mediaType  :: String,
    _image      :: LBS.ByteString
} deriving (Generic, Show)


data Creator = Creator {
    _name       :: String,
    _fileAs     :: Maybe String
} deriving (Generic, Show)


data Book = Book {
    _path       :: FilePath,
    _maybeCover :: Maybe Cover,
    _titles     :: [String],
    _creators   :: [Creator],
    _dates      :: [String],
    _publishers :: [String]
} deriving (Generic, Show)



-- * Instances


instance JSON.ToJSON LBS.ByteString where
    toJSON = JSON.toJSON . Char8.unpack . Base64.encode

instance JSON.ToJSON Cover
instance JSON.ToJSON Creator
instance JSON.ToJSON Book



-- * Settings


bookDirectory = "media/books"



-- * Start of main program


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = do
    paths <- map ((bookDirectory ++ "/") ++ ) <$> listDirectory bookDirectory
    books <- rights <$> mapM readBook paths

    -- Outputting headers and a body to stdout is CGI compatible
    putStrLn "Content-type: text/json; charset=UTF-8\n"
    LBS.putStr $ JSON.encode books


-- Attempt to create a Book from a file path to an epub file
readBook :: FilePath -> IO (Either String Book)
readBook path = runErrorT $ do
    xmlString       <- Epub.getPkgXmlFromZip path
    manifest        <- Epub.getManifest xmlString
    package         <- Epub.getPackage xmlString
    metadata        <- Epub.getMetadata xmlString

    maybeCoverImage <- liftIO $ getCoverImage path manifest

    let titles      = map Epub.titleText $ Epub.metaTitles metadata
        creators    = map toCreator $ Epub.metaCreators metadata
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

    where
        toCreator creator =
            Creator (Epub.creatorText creator) (Epub.creatorFileAs creator)



-- * Cover images


-- Given an archive path and a epub manifest, try to find a cover image
getCoverImage :: FilePath -> Epub.Manifest -> IO (Maybe Cover)
getCoverImage archivePath manifest = do
    archive <- Zip.toArchive <$> LBS.readFile archivePath

    return $ do
        manifestItem <- getCoverManifestItem manifest
        image        <- findFileInArchive archive $ Epub.mfiHref manifestItem
        Just $ Cover (Epub.mfiMediaType manifestItem) image


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


-- * Utils


maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing
