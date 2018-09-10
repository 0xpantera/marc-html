{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Author = T.Text
type Title = T.Text

data Book = Book {
  author :: Author,
  tile :: Title } deriving Show

type Html = T.Text


bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n",
                           titleInTags,
                           authorInTags,
                           "</p>\n"]
  where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
        authorInTags = mconcat ["<em>", (author book), "</em>\n"]
