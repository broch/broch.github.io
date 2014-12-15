{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (msum)
import           Data.Monoid ((<>))
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import           Data.Time.Format (parseTime, formatTime)
import           Data.Time.Clock (UTCTime (..))
import           Hakyll
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)
import           System.Locale (TimeLocale, defaultTimeLocale, iso8601DateFormat)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= removeIndexHtml

    match "posts/*.md" $ do
        route niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= removeIndexHtml


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
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    updatedField "updated" "%B %e, %Y" <>
    updatedField "updated.iso8601" (iso8601DateFormat Nothing) <>
    dateField "date" "%B %e, %Y" <>
    dateField "date.iso8601" (iso8601DateFormat Nothing) <>
    defaultContext

updatedField :: String -> String -> Context a
updatedField key format = field key $ \i -> do
    time <- getUpdatedTime locale $ itemIdentifier i
    return $ formatTime locale format time
  where
    locale = defaultTimeLocale

getUpdatedTime :: MonadMetadata m => TimeLocale -> Identifier -> m UTCTime
getUpdatedTime locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
    maybe empty' return $ msum [tryField "updated" fmt | fmt <- formats]
  where
    empty'     = fail $ "getUpdatedTime: " ++ "could not parse time for " ++ show id'
    parseTime' = parseTime locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]



-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
    (dir, "index.html") | isLocal dir -> dir
                        | otherwise   -> url
    _                                 -> url
  where isLocal uri = not $ "://" `isInfixOf` uri

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident
