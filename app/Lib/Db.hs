{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Db where

import Data.ByteString (ByteString)
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Text.Mustache

dbPoolConfig :: ByteString -> PoolConfig Connection
dbPoolConfig conStr = defaultPoolConfig (connectPostgreSQL conStr) close 30 10

data Business = Business
  { businessId :: Integer
  , businessIdentifier :: Text
  , businessName :: Text
  }
  deriving (Generic, Show, FromRow)

instance ToMustache Business where
  toMustache business =
    object
      [ "businessId" ~> businessId business
      , "businessIdentifier" ~> businessIdentifier business
      , "businessName" ~> businessName business
      ]

data Result a = Result
  { total :: Integer
  , results :: [a]
  }
  deriving (Show)

instance (ToMustache a) => ToMustache (Result a) where
  toMustache result =
    object
      [ "total" ~> total result
      , "results" ~> results result
      ]
