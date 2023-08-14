{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Concurrent.STM
import Data.ByteString.Char8 qualified as C
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Lib.Db
import Lib.Query
import Lib.Utils as U
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static
import System.Environment (getEnv, lookupEnv)
import Text.Mustache
import Text.Mustache.Compile (TemplateCache)
import Web.Scotty as S

newTemplateCache :: IO (TVar TemplateCache)
newTemplateCache = newTVarIO mempty

app :: Pool Connection -> TVar TemplateCache -> ScottyM ()
app dbPool templateCacheTVar = do
  middleware $ staticPolicy $ addBase "public"
  middleware $ staticPolicy $ addBase "dist"

  S.get "" $ do
    renderCompiled templateCacheTVar "index.mustache" ()

  S.get "/about" $ do
    renderHxPage templateCacheTVar "about.mustache" ()

  S.get "/businesses" $ do
    cursor <- getCursorParam
    search <- lookupParam "search" >>= (\x -> if x == Just "" then return Nothing else return x) . fmap T.strip

    result <-
      case search of
        Nothing -> getBusinessResult dbPool cursor
        Just term -> getBusinessSearchResult dbPool term cursor

    let page = getPage cursor
        maxPage = total result `div` getSize cursor + 1

    renderHxPage templateCacheTVar "businesses/businesses.mustache" $
      object
        [ "page" ~> getPage cursor
        , "maxPage" ~> maxPage
        , "disableFirst" ~> (page == 1)
        , "disableLast" ~> (page == maxPage)
        , "nextPage" ~> (page + 1)
        , "prevPage" ~> (page - 1)
        , "results" ~> results result
        , "search" ~> search
        ]

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (return ())

  pgConn <- getEnv "PG_DSN"
  dbPool <- newPool $ dbPoolConfig $ C.pack pgConn

  port <- maybe 8000 read <$> lookupEnv "PORT"
  isDebug <- maybe False (`elem` ["1", "true"]) <$> lookupEnv "DEBUG"

  cache <- newTemplateCache

  scotty port $ do
    if isDebug
      then middleware logStdoutDev
      else middleware logStdout
    middleware simpleCors
    app dbPool cache
