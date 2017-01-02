{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Manifest as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Network.CGI as CGI
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Html5.Attributes

import Control.Monad (forM_)
import Control.Monad.Error (runErrorT, liftIO)
import Data.Either (rights)
import Data.List (isInfixOf)
import Data.String (fromString)
import System.Directory (listDirectory)
import Text.Blaze.Renderer.Utf8 (renderMarkup)


data Book = Book {
    _path     :: FilePath,
    _package  :: Epub.Package,
    _metadata :: Epub.Metadata,
    _manifest :: Epub.Manifest
} deriving (Show)


bookDirectory = "media/books"


-- Read books, run CGI to display them

main :: IO ()
main = do
    paths <- map ((bookDirectory ++ "/") ++ ) <$> listDirectory bookDirectory
    books <- rights <$> mapM readBook paths

    displayHtmlAsCGI $ booksToHtml books


readBook :: FilePath -> IO (Either String Book)
readBook path = runErrorT $ do
    xmlString <- Epub.getPkgXmlFromZip path

    Book
        <$> return path
        <*> Epub.getPackage xmlString
        <*> Epub.getMetadata xmlString
        <*> Epub.getManifest xmlString


displayHtmlAsCGI :: Html5.Html -> IO ()
displayHtmlAsCGI f = CGI.runCGI $ do
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

    Html5.p $ Html5.toHtml $ show $ getCoverImagePath $ _manifest book


getCoverImagePath :: Epub.Manifest -> Maybe String
getCoverImagePath (Epub.Manifest items) =
    case filter (\item -> isImage item && isCover item) items of
        (item:_) -> Just $ Epub.mfiHref item
        _        -> Nothing

    where
        isImage manifestItem = "image" `isInfixOf` Epub.mfiMediaType manifestItem
        isCover manifestItem = "cover" `isInfixOf` Epub.mfiHref manifestItem
