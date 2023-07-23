{-# LANGUAGE OverloadedStrings #-}

module Lib.Pages where

import Data.Text (Text)
import Lib.Db
import Lib.Utils
import Text.Blaze.Html5 (Attribute, AttributeValue, Html, customAttribute, stringValue, textTag, toHtml, (!), (!?))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Printf

hx :: Text -> AttributeValue -> Attribute
hx attr = customAttribute (textTag $ "hx-" <> attr)

hxGetPage :: (PrintfArg t) => t -> Attribute
hxGetPage page = hx "get" (stringValue $ printf "/businesses?page=%u" page)

pageButton :: Bool -> Integer -> Html -> Html
pageButton active page children = do
  H.button
    ! A.class_ "join-item btn"
    !? (not active, A.disabled mempty)
    ! hxGetPage page
    $ children

pageButtons :: Integer -> Integer -> Html
pageButtons page maxPage = H.div ! A.class_ "join" ! hx "target" "#output" $ do
  pageButton (page > 1) 1 "«"
  pageButton (page > 1) (page - 1) "‹"
  H.button ! A.class_ "join-item btn" $ toHtml (printf "Page %u" page :: String)
  pageButton (page < maxPage) (page + 1) "›"
  pageButton (page < maxPage) maxPage "»"

businessRow :: Business -> Html
businessRow (Business _ bid bname) = H.tr $ do
  H.th $ H.toHtml bid
  H.td $ H.toHtml bname

businessSearch :: Html
businessSearch =
  H.input
    ! A.type_ "search"
    ! A.name "search"
    ! A.placeholder "Search"
    ! A.class_ "input input-bordered w-full max-w-xs"
    ! hx "post" "/businesses"
    ! hx "trigger" "keyup changed delay:500ms, search"
    ! hx "target" "#business-table"

businessTable :: [Business] -> Html
businessTable businesses = do
  H.table ! A.class_ "table table-pin-rows table-sm" $ do
    H.thead $ H.tr $ do
      H.th "ID"
      H.th "Name"
    H.tbody $ mapM_ businessRow businesses

businessPaginatedTable :: Cursor -> Result Business -> Html
businessPaginatedTable (Cursor page size) Result{count = c, results = bs} = do
  let maxPage = c `div` size + 1

  H.div ! A.id "business-table" $ do
    pageButtons page maxPage
    businessTable bs

businessPage :: Cursor -> Result Business -> Html
businessPage cursor result = do
  H.div ! A.class_ "overflow-x-auto" $ do
    businessSearch
    businessPaginatedTable cursor result
