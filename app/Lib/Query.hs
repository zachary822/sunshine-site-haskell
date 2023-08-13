{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Query where

import Control.Monad.Trans.Class
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lib.Db
import Lib.Utils
import Text.Printf (printf)

getBusinessResult :: (MonadTrans t) => Pool Connection -> Cursor -> t IO (Result Business)
getBusinessResult dbPool cursor = lift $ withResource dbPool $ \conn -> withTransaction conn $ do
  let limit = getSize cursor
      offset = (getPage cursor - 1) * limit
  businesses <-
    query
      conn
      [sql|
        select id, business_identifier, name from business order by business_identifier limit ? offset ?
      |]
      (limit, offset)
  [Only count] <-
    query_
      conn
      [sql|
        select count(*) from business
      |]

  return $ Result count businesses

getBusinessSearchResult :: (MonadTrans t) => Pool Connection -> Text -> Cursor -> t IO (Result Business)
getBusinessSearchResult dbPool term cursor = lift $ withResource dbPool $ \conn -> withTransaction conn $ do
  let like :: String
      like = printf "%%%s%%" term
      limit = getSize cursor
      offset = (getPage cursor - 1) * limit

  businesses <-
    query
      conn
      [sql|
        select id, business_identifier, name
        from business
        where name like ? or business_identifier like ?
        order by business_identifier
        limit ? offset ?
      |]
      (like, like, limit, offset)
  [Only count] <-
    query
      conn
      [sql|
        select count(id)
        from business
        where name like ? or business_identifier like ?
      |]
      (like, like)

  return $ Result count businesses
