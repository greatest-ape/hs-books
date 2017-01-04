{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
-- import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Network.CGI as CGI

import Control.Monad (forM_)
import Control.Monad.Error (runErrorT, liftIO)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isInfixOf, nub)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import System.Directory (listDirectory)



data Cover = Cover {
    _mediaTypes :: String,
    _image      :: LBS.ByteString
} deriving (Show)


data Book = Book {
    _path       :: FilePath,
    _maybeCover :: Maybe Cover,
    _titles     :: [String],
    _creators   :: [Epub.Creator],
    _dates      :: [String],
    _publishers :: [String]
} deriving (Show)


bookDirectory = "media/books"


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = do
    paths <- map ((bookDirectory ++ "/") ++ ) <$> listDirectory bookDirectory
    books <- rights <$> mapM readBook paths

    CGI.runCGI $ do
        CGI.setHeader "Content-type" "text/json; charset=UTF-8"
        CGI.outputFPS $ booksToJSON books


-- Attempt to create a Book from a file path to an epub file
readBook :: FilePath -> IO (Either String Book)
readBook path = runErrorT $ do
    xmlString <- Epub.getPkgXmlFromZip path
    manifest  <- Epub.getManifest xmlString
    package   <- Epub.getPackage xmlString
    metadata  <- Epub.getMetadata xmlString

    maybeCoverImage <- liftIO $ getCoverImage path manifest

    let titles     = map Epub.titleText $ Epub.metaTitles metadata
        creators   = Epub.metaCreators metadata
        dates      = map (\(Epub.Date _ text) -> text) $ Epub.metaDates metadata
        publishers = Epub.metaPublishers metadata

    return $ Book {
        _path       = path,
        _maybeCover = maybeCoverImage,
        _titles     = titles,
        _creators   = creators,
        _dates      = dates,
        _publishers = publishers
    }



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
    maybeHead $ filter (\i -> isCover i && isImage i) items

    where
        isImage manifestItem = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        isCover manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)


-- Extract a file in a zip archive by the file path. Looks in all root level
-- folders
findFileInArchive :: Zip.Archive -> FilePath -> Maybe LBS.ByteString
findFileInArchive archive path =
    let folders = nub $ map (takeWhile (/= '/')) $ Zip.filesInArchive archive
        paths   = path : map (\folder -> folder ++ "/" ++ path) folders
        entries = catMaybes $ map (flip Zip.findEntryByPath archive) paths
    in fmap Zip.fromEntry $ maybeHead entries


-- * Generation of JSON from books

booksToJSON books = undefined


-- * Utils

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing
