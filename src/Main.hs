{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Aeson as JSON
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Tree.Class as HXT
import qualified Graphics.GD as GD
import qualified Network.CGI as CGI
import qualified Network.URI.Encode as URI
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.XML.HXT.Core as HXT

import Control.Exception (SomeException, try)
import Control.Arrow
import Control.Monad (forM_)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)
import Data.Aeson ((.=), (.:))
import Data.Char (toLower)
import Data.Either (rights, fromRight, isLeft)
import Data.List (isSuffixOf, isInfixOf, nub, scanl', words, unwords)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust)
import Data.String (fromString)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist)



-- * Settings


bookDirectory           = "media/books"
thumbnailDirectory      = "media/covers"

jpegQuality             = 80

cachePath               = ".cache"

imageMaxWidth  = 16 * 15
imageMaxHeight = 16 * 15



-- * Types and instances


-- Identifies a cover image
type Identifier = String


data Cover = Cover {
    _thumbnailPath :: String
} deriving (Generic, Show)


data Book = Book {
    _path       :: String,
    _maybeCover :: Maybe Cover,
    _titles     :: [String],
    _creators   :: [String],
    _dates      :: [String],
    _publishers :: [String],
    _languages  :: [String],
    _textBytes  :: Integer
} deriving (Generic, Show)


type FileHash = BS.ByteString
type FileHashSet = Set.Set (String, FileHash)

data Cache = Cache {
    _hashes :: FileHashSet,
    _books  :: Map.Map String Book
} deriving (Generic, Show)


instance Binary.Binary Cover
instance Binary.Binary Book
instance Binary.Binary Cache


instance JSON.ToJSON Cover where
    toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- Custom instance to prevent encoding errors
instance JSON.ToJSON Book where
    toJSON book = JSON.object [
        "_path" .= (Text.decodeUtf8 $ Char8.pack $ _path book),
        "_maybeCover" .= _maybeCover book,
        "_titles" .= _titles book,
        "_creators" .= _creators book,
        "_dates" .= _dates book,
        "_publishers" .= _publishers book,
        "_languages" .= _languages book,
        "_textBytes" .= _textBytes book
        ]
    toEncoding book = JSON.pairs (
        "_path" .= (Text.decodeUtf8 $ Char8.pack $ _path book) <>
        "_maybeCover" .= _maybeCover book <>
        "_titles" .= _titles book <>
        "_creators" .= _creators book <>
        "_dates" .= _dates book <>
        "_publishers" .= _publishers book <>
        "_languages" .= _languages book <>
        "_textBytes" .= _textBytes book
        )



-- * Start of main program


-- Read epub files, parse them into the Book datatype, convert them to JSON
-- and send them to the client through CGI
main :: IO ()
main = CGI.runCGI $ CGI.handleErrors $ do
    currentFilenames <- map
        (\f -> bookDirectory ++ "/" ++ f) .
        filter (".epub" `isSuffixOf`) <$>
        (CGI.liftIO $ listDirectory bookDirectory)

    currentFileHashes <- map MD5.hashlazy <$>
        liftIO (mapM LBS.readFile currentFilenames)

    let currentHashes = Set.fromList $ zip currentFilenames currentFileHashes

    forceReload <- isJust <$> CGI.getInput "force-reload"
    forceReloadFull <- isJust <$> CGI.getInput "force-reload-full"
    oldOrEmptyCache <- fromRight createEmptyCache <$> liftIO readCache

    books <- getBooksWithFilenameComparison forceReload currentHashes $
        if forceReloadFull then createEmptyCache else oldOrEmptyCache

    -- CGI output

    CGI.setHeader "Content-type" "application/json"
    CGI.outputFPS $ JSON.encode books

    where

        createEmptyCache :: Cache
        createEmptyCache = Cache {
            _hashes = Set.empty,
            _books = Map.empty
        }

        -- TODO improve code
        readCache :: IO (Either SomeException Cache)
        readCache = try $
            fromRight createEmptyCache <$> Binary.decodeFileOrFail cachePath

        -- If filenames haven't changed, just return cached books.
        -- Otherwise, call getBooksAndUpdateCache
        getBooksWithFilenameComparison
            :: Bool
            -> FileHashSet
            -> Cache
            -> CGI.CGI [Book]
        getBooksWithFilenameComparison forceReload currentHashes oldCache = do
            let oldFilenames = Set.map fst $ _hashes oldCache
            let newFilenames = Set.map fst currentHashes

            if oldFilenames == newFilenames && not forceReload
                then return $ Map.elems $ _books oldCache
                else getBooksAndUpdateCache currentHashes oldCache

        -- Process any new books, save cache, return new books
        getBooksAndUpdateCache
            :: FileHashSet
            -> Cache
            -> CGI.CGI [Book]
        getBooksAndUpdateCache currentHashes oldCache = do
            let oldHashes = _hashes oldCache
            let oldBooks = _books oldCache
            let newHashes = Set.difference currentHashes oldHashes
            let unchangedHashes = Set.intersection currentHashes oldHashes

            newBookList <- readBooks $ map fst $ Set.toList newHashes

            let unchangedBookList = catMaybes $ map
                    (\filename -> Map.lookup filename oldBooks)
                    (map fst $ Set.toList unchangedHashes)

            let newBookMap = Map.fromList $
                    map (\b -> (_path b, b)) newBookList
            let unchangedBookMap = Map.fromList $
                    map (\b -> (_path b, b)) unchangedBookList
            let fullBookMap = Map.union newBookMap unchangedBookMap

            let cache = Cache {
                _hashes = Set.union unchangedHashes newHashes,
                _books  = fullBookMap
            }

            liftIO $ Binary.encodeFile cachePath cache

            return $ Map.elems fullBookMap

        readBooks :: [String] -> CGI.CGI [Book]
        readBooks filePaths = rights <$>
            (CGI.liftIO $ mapM readBook filePaths)


-- Attempt to create a Book from a file path to an epub file
readBook :: String -> IO (Either String Book)
readBook archivePath = runErrorT $ do
    let path       = archivePath
        hash       = MD5.hashlazy $ fromString path
        identifier = Char8.unpack $ Base16.encode hash

    xmlString       <- Epub.getPkgXmlFromZip path
    manifest        <- Epub.getManifest xmlString
    package         <- Epub.getPackage xmlString
    metadata        <- Epub.getMetadata xmlString

    maybeCoverTag   <- liftIO $ getMetaCoverTag xmlString
    textBytes       <- liftIO $ getNumberOfTextBytes path

    eitherMaybeCoverImage <- liftIO $ getCoverImage path manifest maybeCoverTag identifier
    let maybeCoverImage = case eitherMaybeCoverImage of
            Right maybeCoverImage -> maybeCoverImage
            Left _                -> Nothing

    let titles      = map Epub.titleText $ Epub.metaTitles metadata
        creators    = map extractNameWithComma $ Epub.metaCreators metadata
        dates       = map (\(Epub.Date _ text) -> text) $ Epub.metaDates metadata
        publishers  = Epub.metaPublishers metadata
        languages   = Epub.metaLangs metadata

    return $ Book {
        _path       = archivePath,
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

-- Look for meta tag with name="cover" pointing to cover image.
-- Unfortunately runs in the IO monad because of HXT
getMetaCoverTag :: String -> IO (Maybe String)
getMetaCoverTag xmlString = do
    results <- HXT.runX $
        HXT.configSysVars []
        >>>
        HXT.readString [] xmlString
        >>>
        HXT.processChildren (HXT.deep $
            HXT.isElem >>> HXT.hasName "meta"
            >>>
            HXT.hasAttrValue "name" (isInfixOf "cover" . map toLower)
            >>>
            HXT.getAttrValue0 "content" >>> HXT.mkText
            )

    -- Extract one text value from tree or nothing
    return $ maybeHead $ results >>= foldr getHxtText []

    where
        getHxtText (HXT.XText s) acc = s : acc
        getHxtText _             acc = acc


-- Given an archive path, a epub manifest and a string identifier try to find
-- a cover image
-- If found, save a thumbnail and return a Cover
getCoverImage
    :: FilePath
    -> Epub.Manifest
    -> Maybe String
    -> Identifier
    -> IO (Either SomeException (Maybe Cover))
getCoverImage archivePath manifest maybeCoverTag identifier = try $ do
    let thumbnailPath  = thumbnailDirectory ++ "/" ++ identifier ++ ".jpg"
        justCover      = Just $ Cover thumbnailPath

    fileExists <- doesFileExist thumbnailPath

    if fileExists
        then return justCover
        else do
            imageData <- getImageData archivePath maybeCoverTag manifest

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
    -> Maybe String
    -> Epub.Manifest
    -> IO (Maybe (String, LBS.ByteString))
getImageData archivePath maybeCoverTag manifest = do
    archive <- Zip.toArchive <$> LBS.readFile archivePath

    return $ do
        manifestItem    <- getCoverManifestItem maybeCoverTag manifest
        maybeByteString <- tryTwo
            (findFileInArchive archive $ Epub.mfiHref manifestItem)
            (findFileInArchive archive $ URI.decode $ Epub.mfiHref manifestItem)

        Just (Epub.mfiMediaType manifestItem, maybeByteString)
    
    where
        tryTwo (Just a) _ = Just a
        tryTwo (Nothing) b = b


-- Given an epub manifest, attempts to find a ManifestItem for a cover image
getCoverManifestItem
    :: Maybe String
    -> Epub.Manifest
    -> Maybe Epub.ManifestItem
getCoverManifestItem maybeCoverTag (Epub.Manifest items) =
    maybeHead $ filter (\i ->
        isImage i && (
            hasCoverInHref i ||
            hasCoverInID i ||
            matchesMetaTag i maybeCoverTag
            )
        ) items
    where
        isImage manifestItem =
            "image" `isInfixOf` Epub.mfiMediaType manifestItem

        hasCoverInHref manifestItem =
            "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)

        hasCoverInID manifestItem =
            "cover" `isInfixOf` map toLower (Epub.mfiId manifestItem)

        matchesMetaTag manifestItem (Just t) = t == Epub.mfiId manifestItem
        matchesMetaTag manifestItem Nothing  = False


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
