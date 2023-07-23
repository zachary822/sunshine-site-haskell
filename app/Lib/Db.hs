{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Db where

import Data.ByteString
import Data.Pool
import Data.Text
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

dbPoolConfig :: ByteString -> PoolConfig Connection
dbPoolConfig conStr = defaultPoolConfig (connectPostgreSQL conStr) close 30 10

data Business = Business
  { businessId :: Integer
  , businessIdentifier :: Text
  , businessName :: Text
  }
  deriving (Generic, Show, FromRow)

data Result a = Result
  { count :: Integer
  , results :: [a]
  }
  deriving (Show)
