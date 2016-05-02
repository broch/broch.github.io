{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (msum)
import           Data.Monoid ((<>))
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Time.Format (parseTimeM, formatTime, TimeLocale, defaultTimeLocale, iso8601DateFormat)
import           Data.Time.Clock (UTCTime (..))
import           Hakyll
import           System.Environment
import           System.FilePath.Posix  (takeBaseName, takeDirectory, (</>), splitFileName)
import           Text.Pandoc.Options (writerExtensions, Extension(Ext_literate_haskell))

config :: Configuration
config = defaultConfiguration
    { deployCommand = "./deploy.sh deploy"
    }

main :: IO ()
main = do
    args <- getArgs
    let isPreview action = action == "watch" || action == "preview"
        preview = not (null args) && isPreview (head args)
        mdOrLhs d = d <> ("**.md" .||. "**.lhs")
        postsPattern
          | preview = mdOrLhs "posts/*" .||. mdOrLhs "drafts/*"
          | otherwise = mdOrLhs "posts/*"

    hakyllWith config $ do
        match ("images/*" .||. "CNAME") $ do
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

        match postsPattern $ do
            route niceRoute
            compile $ pandocCompilerWith defaultHakyllReaderOptions outputOptionsSansLHS
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
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
                posts <- recentFirst =<< loadAll postsPattern
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
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

outputOptionsSansLHS = defaultHakyllWriterOptions
    { writerExtensions = S.delete Ext_literate_haskell (writerExtensions defaultHakyllWriterOptions)
    }

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
    (dir, "index.html") | isLocal dir -> dir
                        | otherwise   -> url
    _                                 -> url
  where
    isLocal :: String -> Bool
    isLocal uri = not $ "://" `isInfixOf` uri

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident
