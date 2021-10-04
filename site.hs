{-# LANGUAGE OverloadedStrings #-}
import           Data.List             (isSuffixOf)
import           Hakyll
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import qualified GHC.IO.Encoding as E

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

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8

  hakyll $ do
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

    create ["writings.html"] $ do
        route     cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let writingCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Writings"            <>
                  defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/writings.html" writingCtx
                >>= loadAndApplyTemplate "templates/default.html"  writingCtx
                >>= cleanIndexUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match "*.md" $ do
        route     cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match "404.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
