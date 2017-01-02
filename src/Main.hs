{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.CGI as CGI
import qualified Codec.Epub as Epub
import qualified Codec.Epub.Data.Metadata as Epub
import qualified Codec.Epub.Data.Package as Epub
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Html5

import Control.Monad (forM_)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)
import Data.List (isInfixOf)
import System.Directory (listDirectory)
import Text.Blaze.Renderer.Utf8 (renderMarkup)



data Book = Book {
    _path     :: FilePath,
    _package  :: Epub.Package,
    _metadata :: Epub.Metadata
} deriving (Show)


bookDirectory = "books"

main :: IO ()
main = do
    paths <- map ((bookDirectory ++ "/") ++ ) <$> listDirectory bookDirectory
    eitherBooks <- runErrorT $ mapM readBook paths

    case eitherBooks of
        Left  err   -> putStrLn "Error while reading books"
        Right books -> cgiDisplayBooks books


readBook :: FilePath -> ErrorT String IO Book
readBook path = do
    xmlString <- Epub.getPkgXmlFromZip path

    Book
        <$> return path
        <*> Epub.getPackage xmlString
        <*> Epub.getMetadata xmlString


cgiDisplayBooks :: [Book] -> IO ()
cgiDisplayBooks books = CGI.runCGI $ CGI.outputFPS $ renderMarkup $ do
    Html5.docTypeHtml $ do
        Html5.body $ do
            forM_ books $ \book -> do
                Html5.p $ Html5.toHtml $ bookToHtml book


bookToHtml :: Book -> Html5.Html
bookToHtml book = Html5.div $ do
    let titles      = map Epub.titleText $ Epub.metaTitles $ _metadata book
        dates       = map (\(Epub.Date _ date) -> date) $ Epub.metaDates $ _metadata book
        publishers  = Epub.metaPublishers $ _metadata book
        languages   = Epub.metaLangs $ _metadata book

        -- Removed because it doesn't work for all ePub files
        -- creators    = Epub.metaCreators $ _metadata book
        -- authors     = map Epub.creatorText $ filter (roleContains "aut") creators
        -- translators = map Epub.creatorText $ filter (roleContains "trl") creators

        creators    = map Epub.creatorText $ Epub.metaCreators $ _metadata book

    forM_ titles $ (Html5.h2 Html5.! Html5.class_ "title") . Html5.toHtml
    forM_ creators $ (Html5.h2 Html5.! Html5.class_ "creator") . Html5.toHtml

    -- where
    --     roleContains s creator =
    --         case Epub.creatorRole creator of
    --             Just role -> s `isInfixOf` role
    --             Nothing   -> False

