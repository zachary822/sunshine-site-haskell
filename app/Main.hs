{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Data.ByteString.Char8 qualified as C
import Data.Pool
import Database.PostgreSQL.Simple
import Lib.Db (dbPoolConfig)
import Lib.Pages
import Lib.Query
import Lib.Utils as U
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import System.Environment (getEnv)
import Web.Scotty as S

app :: Pool Connection -> ScottyM ()
app dbPool = do
  middleware logStdoutDev
  middleware $ staticPolicy $ U.index "index.html" <> addBase "public"
  middleware $ staticPolicy $ addBase "dist"

  S.get "/businesses" $ do
    cursor <- getCursorParam

    result <- getBusinessResult dbPool cursor

    U.html $ businessTable cursor result

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (return ())

  pgConn <- getEnv "PG_DSN"
  dbPool <- newPool $ dbPoolConfig $ C.pack pgConn

  scotty 3000 $ app dbPool
