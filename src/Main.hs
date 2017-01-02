{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.CGI as CGI
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Html5.Attributes

import Control.Monad (forM_)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)
import Data.List (isInfixOf)
import Data.Either (rights)
import System.Directory (listDirectory)
import Text.Blaze.Renderer.Utf8 (renderMarkup)


data Book = Book {
    _path     :: FilePath,
    _package  :: Epub.Package,
    _metadata :: Epub.Metadata
} deriving (Show)


bookDirectory = "books"


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
        Html5.body $ do
            forM_ books $ \book -> do
                Html5.p $ Html5.toHtml $ bookToHtml book


bookToHtml :: Book -> Html5.Html
bookToHtml book = (Html5.div Html5.! Html5.Attributes.class_ "book") $ do
    let titles      = map Epub.titleText $ Epub.metaTitles $ _metadata book
        creators    = map Epub.creatorText $ Epub.metaCreators $ _metadata book
        dates       = map (\(Epub.Date _ date) -> date) $ Epub.metaDates $ _metadata book
        publishers  = Epub.metaPublishers $ _metadata book
        languages   = Epub.metaLangs $ _metadata book

    forM_ (take 1 titles) $
        (Html5.h2 Html5.! Html5.Attributes.class_ "title") . Html5.toHtml

    forM_ (take 1 creators) $
        (Html5.h2 Html5.! Html5.Attributes.class_ "creator") . Html5.toHtml
