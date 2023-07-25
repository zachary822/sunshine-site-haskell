{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Data.ByteString.Char8 qualified as C
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Lib.Db
import Lib.Pages.Businesses
import Lib.Query
import Lib.Utils as U
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static
import System.Environment (getEnv, lookupEnv)
import Web.Scotty as S

app :: Pool Connection -> ScottyM ()
app dbPool = do
  middleware $ staticPolicy $ addBase "public"
  middleware $ staticPolicy $ addBase "dist"

  S.get "" $ do
    file "public/index.html"

  S.get "/businesses" $ do
    U.redirectNonHtmx

    cursor <- getCursorParam
    result <- getBusinessResult dbPool cursor
    U.html $ businessPage cursor result

  S.post "/businesses" $ do
    U.redirectNonHtmx

    search <- T.strip <$> S.param "search" `rescue` (const $ raiseStatus status404 "not found")
    S.status status200
    if T.null search
      then do
        result <- getBusinessResult dbPool defaultCursor
        U.html $ businessPaginatedTable defaultCursor result
      else do
        Result{results = bs} <- getBusinessSearchResult dbPool search
        U.html $ businessTable 1 bs

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (return ())

  pgConn <- getEnv "PG_DSN"
  dbPool <- newPool $ dbPoolConfig $ C.pack pgConn

  port <- lookupEnv "PORT" >>= return . maybe 8000 read
  isDebug <- lookupEnv "DEBUG" >>= return . maybe False (flip elem ["1", "true"])

  scotty port $ do
    if isDebug
      then middleware logStdoutDev
      else middleware logStdout
    middleware simpleCors
    app dbPool
