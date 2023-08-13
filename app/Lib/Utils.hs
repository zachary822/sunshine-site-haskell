{-# LANGUAGE OverloadedStrings #-}

module Lib.Utils where

import Control.Monad
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.Static (Policy, policy)
import Web.Scotty

indexName :: String -> String -> Maybe String
indexName name "" = Just name
indexName _ path = Just path

index :: String -> Policy
index name = policy $ indexName name

defaultParam :: (Parsable a) => Text -> a -> ActionM a
defaultParam name def = param name `rescue` (\_ -> return def)

data Cursor = Cursor
  { getPage :: Integer
  , getSize :: Integer
  }
  deriving (Show)

defaultCursor :: Cursor
defaultCursor = Cursor 1 50

getCursorParam :: ActionM Cursor
getCursorParam = do
  size <- defaultParam "size" 50
  page <- defaultParam "page" 1

  let sizeLowerBound = 1
      sizeUpperBound = 100
      pageLowerBound = 1

  guard (sizeLowerBound <= size && size <= sizeUpperBound)
  guard (pageLowerBound <= page)

  return
    Cursor
      { getSize = size
      , getPage = page
      }

isHxRequest :: ActionM Bool
isHxRequest = (Just "true" ==) <$> header "hx-request"

redirectNonHtmx :: ActionM ()
redirectNonHtmx = do
  isHtmx <- isHxRequest
  unless isHtmx (redirect "/")
