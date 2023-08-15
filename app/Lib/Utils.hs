{-# LANGUAGE OverloadedStrings #-}

module Lib.Utils where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Data.HashMap.Strict (union)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Network.Wai.Middleware.Static (Policy, policy)
import System.FilePath
import Text.Mustache
import Text.Mustache.Compile (TemplateCache, cacheFromList)
import Web.Scotty

lookupParam :: (Parsable a) => Text -> ActionM (Maybe a)
lookupParam t = (Just <$> param t) `rescue` const (return Nothing)

searchSpace :: [String]
searchSpace = [".", "./templates"]

renderCompiled :: (ToMustache k) => TVar TemplateCache -> FilePath -> k -> ActionM ()
renderCompiled templateCacheTVar name values = do
  cache <- lift $ readTVarIO templateCacheTVar
  compiled <- lift $ compileTemplateWithCache searchSpace cache name

  case compiled of
    Left err -> raise (T.pack $ show err)
    Right template -> do
      let cache' = cacheFromList [template]
      lift $ atomically $ modifyTVar' templateCacheTVar (union cache')
      html . T.fromStrict $ substitute template values

renderFullCompiled :: (ToMustache k) => TVar TemplateCache -> FilePath -> k -> ActionM ()
renderFullCompiled templateCacheTVar name values = do
  isHx <- isHxRequest

  if isHx
    then renderCompiled templateCacheTVar name values
    else do
      let (pref, ext) = splitExtension name
      renderCompiled templateCacheTVar (pref <> "_full" <.> ext) values

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
isHxRequest = (Just "true" ==) <$> header "HX-Request"

redirectNonHtmx :: ActionM ()
redirectNonHtmx = do
  isHtmx <- isHxRequest
  unless isHtmx (redirect "/")
