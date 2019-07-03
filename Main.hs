{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Char (toUpper)
import           Data.List (isSuffixOf)
import           Data.String
import           Data.Monoid ((<>))
import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))


--------------------------------------------------------------------------------

-- From https://www.rohanjain.in/hakyll-clean-urls/

createIndexRoute :: Identifier -> FilePath
createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
  where p = toFilePath ident

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

-- cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

--------------------------------------------------------------------------------

pages :: IsString a => [a]
pages = [ "about.markdown"
        , "academia.markdown"
        ]

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route     cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= cleanIndexUrls

    create ["archive.html"] $ do
        route     cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Archives"            <>
                  defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= cleanIndexUrls

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match "404.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match (fromList pages) $ do
        route     cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Home"                <>
                  defaultContext

            getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

