{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Data.ByteString.Char8 qualified as C
import Data.Pool
import Data.Text (strip)
import Database.PostgreSQL.Simple
import Lib.Db
import Lib.Pages
import Lib.Query
import Lib.Utils as U
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import System.Environment (getEnv, lookupEnv)
import Web.Scotty as S

app :: Pool Connection -> ScottyM ()
app dbPool = do
  middleware $ staticPolicy $ U.index "index.html" <> addBase "public"
  middleware $ staticPolicy $ addBase "dist"

  S.get "/businesses" $ do
    cursor <- getCursorParam

    result <- getBusinessResult dbPool cursor

    U.html $ businessPage cursor result

  S.post "/businesses" $ do
    search <- strip <$> S.param "search" `rescue` \_ -> raiseStatus status404 "not found"
    case search of
      "" -> do
        result <- getBusinessResult dbPool defaultCursor
        U.html $ businessPaginatedTable defaultCursor result
      _ -> do
        Result{results = bs} <- getBusinessSearchResult dbPool search
        U.html $ businessTable bs

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (return ())

  pgConn <- getEnv "PG_DSN"
  dbPool <- newPool $ dbPoolConfig $ C.pack pgConn

  port <- lookupEnv "PORT" >>= return . maybe 8000 read

  scotty port $ do
    middleware logStdoutDev
    app dbPool
