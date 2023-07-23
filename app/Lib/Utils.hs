{-# LANGUAGE OverloadedStrings #-}

module Lib.Utils where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.Static (Policy, policy)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Scotty

html :: Html -> ActionM ()
html = raw . renderHtml

indexName :: String -> String -> Maybe String
indexName name "" = Just name
indexName _ path = Just path

index :: String -> Policy
index name = policy $ indexName name

maybeParam :: (Parsable a) => Text -> a -> MaybeT ActionM a
maybeParam name def = MaybeT $ (Just <$> param name) `rescue` (\_ -> return $ Just def)

data Cursor = Cursor
  { getPage :: Integer
  , getSize :: Integer
  }
  deriving (Show)

getCursorParam :: ActionM Cursor
getCursorParam = do
  result <- runMaybeT $ do
    size <- maybeParam "size" 50
    page <- maybeParam "page" 1

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

  case result of
    Nothing -> raise "Either limit or offset parameter is out of bounds."
    Just cursor -> return cursor
