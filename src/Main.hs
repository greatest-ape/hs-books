{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Data.ByteString.Lazy as LBS
import qualified Network.CGI as CGI
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Html5.Attributes

import Control.Monad (forM_)
import Control.Monad.Error (runErrorT, liftIO)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isInfixOf, nub)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import System.Directory (listDirectory)
import Text.Blaze.Renderer.Utf8 (renderMarkup)


data Book = Book {
    _path            :: FilePath,
    _maybeCoverImage :: Maybe LBS.ByteString,
    _package         :: Epub.Package,
    _metadata        :: Epub.Metadata
} deriving (Show)


bookDirectory = "media/books"


-- Read books, extract info and cover images, run CGI to display data

main :: IO ()
main = do
    paths <- map ((bookDirectory ++ "/") ++ ) <$> listDirectory bookDirectory
    books <- rights <$> mapM readBook paths

    sendHtmlThroughCGI $ booksToHtml books


readBook :: FilePath -> IO (Either String Book)
readBook path = runErrorT $ do
    xmlString <- Epub.getPkgXmlFromZip path
    manifest  <- Epub.getManifest xmlString

    Book
        <$> return path
        <*> liftIO (getCoverImage path manifest)
        <*> Epub.getPackage xmlString
        <*> Epub.getMetadata xmlString


getCoverImage :: FilePath -> Epub.Manifest -> IO (Maybe LBS.ByteString)
getCoverImage filePath manifest =
    case getCoverImagePath manifest of
        Just coverImagePath -> do
            archive <- Zip.toArchive <$> LBS.readFile filePath
            return $! fmap Zip.fromEntry $ findEntry archive coverImagePath
        Nothing -> return Nothing

    where
        -- Look in all root level folders for the coverImagePath
        findEntry :: Zip.Archive -> FilePath -> Maybe Zip.Entry
        findEntry archive coverImagePath =
            let folders = nub $ map (takeWhile (/= '/')) $ Zip.filesInArchive archive
                paths = coverImagePath : map (\folder -> folder ++ "/" ++ coverImagePath) folders
            in maybeHead $ catMaybes $ map (flip Zip.findEntryByPath archive) paths


getCoverImagePath :: Epub.Manifest -> Maybe FilePath
getCoverImagePath (Epub.Manifest items) =
    fmap Epub.mfiHref $ maybeHead $ filter (\i -> isCover i && isImage i) items

    where
        isImage manifestItem = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        isCover manifestItem = "cover" `isInfixOf` map toLower (Epub.mfiHref manifestItem)


sendHtmlThroughCGI :: Html5.Html -> IO ()
sendHtmlThroughCGI f = CGI.runCGI $ do
    CGI.setHeader "Content-type" "text/html; charset=UTF-8"
    CGI.outputFPS $ renderMarkup f


-- * Generation of HTML from books

booksToHtml :: [Book] -> Html5.Html
booksToHtml books = do
    Html5.docTypeHtml $ do
        Html5.head $ do
            Html5.title $ Html5.toHtml ("Books" :: String)

            addCss "static/css/vendor/reset.css"
            addCss "static/css/main.css"

            addJs "static/js/vendor/jquery-3.1.1.min.js"
            addJs "static/js/main.js"

        Html5.body $ Html5.div Html5.! Html5.Attributes.class_ "books" $ do
            Html5.h1 $ Html5.toHtml ("Books" :: String)
            forM_ books bookToHtml

    where
        addCss url =
            Html5.link
                Html5.! Html5.Attributes.rel "stylesheet"
                Html5.! Html5.Attributes.href url

        addJs url =
            Html5.script Html5.! Html5.Attributes.src url $
                Html5.toHtml ("" :: String)

bookToHtml :: Book -> Html5.Html
bookToHtml book = Html5.div Html5.! Html5.Attributes.class_ "book" $ do
    let titles      = map Epub.titleText $ Epub.metaTitles $ _metadata book
        creators    = map Epub.creatorText $ Epub.metaCreators $ _metadata book
        dates       = map (\(Epub.Date _ date) -> date) $ Epub.metaDates $ _metadata book
        publishers  = Epub.metaPublishers $ _metadata book
        languages   = Epub.metaLangs $ _metadata book

    -- Generate title
    forM_ (take 1 titles) $ \title -> do
        (Html5.h2 Html5.! Html5.Attributes.class_ "title") $
            (Html5.a Html5.! Html5.Attributes.href (fromString $ _path book)) $
                Html5.toHtml title

    -- Generate author
    forM_ (take 1 creators) $
        (Html5.h2 Html5.! Html5.Attributes.class_ "creator") . Html5.toHtml

    Html5.p $ Html5.toHtml $ show $ fmap LBS.length $ _maybeCoverImage book


-- * Utils

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing
