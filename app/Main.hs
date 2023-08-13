{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Data.ByteString.Char8 qualified as C
import Data.HashMap.Strict qualified as Map
import Data.Pool
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Database.PostgreSQL.Simple
import Lib.Db
import Lib.Query
import Lib.Utils as U
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static
import System.Environment (getEnv, lookupEnv)
import Text.Mustache
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Web.Scotty as S

searchSpace :: [String]
searchSpace = [".", "./templates"]

newTemplateCache :: IO (TVar TemplateCache)
newTemplateCache = newTVarIO mempty

renderCompiled :: (ToMustache k) => TVar TemplateCache -> FilePath -> k -> ActionM ()
renderCompiled templateCacheTVar name values = do
  cache <- lift $ readTVarIO templateCacheTVar
  compiled <- lift $ compileTemplateWithCache searchSpace cache name

  case compiled of
    Left err -> raise (TL.pack $ show err)
    Right template -> do
      let cache' = cacheFromList [template]
      lift $ atomically $ modifyTVar' templateCacheTVar (Map.union cache')
      S.html . TL.fromStrict $ substitute template values

app :: Pool Connection -> TVar TemplateCache -> ScottyM ()
app dbPool templateCacheTVar = do
  middleware $ staticPolicy $ addBase "public"
  middleware $ staticPolicy $ addBase "dist"

  S.get "" $ do
    renderCompiled templateCacheTVar "index.mustache" ()

  S.get "/businesses" $ do
    U.redirectNonHtmx

    cursor <- getCursorParam
    result <- getBusinessResult dbPool cursor

    let page = getPage cursor
        maxPage = total result `div` getSize cursor + 1

    renderCompiled templateCacheTVar "businesses/businesses.mustache" $
      object
        [ "page" ~> getPage cursor
        , "maxPage" ~> maxPage
        , "disableFirst" ~> (page == 1)
        , "disableLast" ~> (page == maxPage)
        , "nextPage" ~> (page + 1)
        , "prevPage" ~> (page - 1)
        , "results" ~> results result
        ]

  S.post "/businesses" $ do
    U.redirectNonHtmx

    search <- T.strip <$> S.param "search" `rescue` const (raiseStatus status404 "not found")
    S.status status200

    if T.null search
      then do
        result <- getBusinessResult dbPool defaultCursor

        let maxPage = total result `div` getSize defaultCursor + 1
            page = getPage defaultCursor

        renderCompiled templateCacheTVar "businesses/business_search_result.mustache" $
          object
            [ "page" ~> page
            , "maxPage" ~> maxPage
            , "disableFirst" ~> (page == 1)
            , "disableLast" ~> (page == maxPage)
            , "nextPage" ~> (page + 1)
            , "prevPage" ~> (page - 1)
            , "results" ~> results result
            ]
      else do
        result <- getBusinessSearchResult dbPool search
        renderCompiled templateCacheTVar "businesses/business_search_result.mustache" result

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
