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


book1 :: Book
book1 = Book {
  title = "The Conspiracy Against the Human Race",
  author = "Ligotti, Thomas" }

book2 :: Book
book2 = Book {
  title = "A Short History of Decay",
  author = "Cioran, Emil" }

book3 :: Book
book3 = Book {
  title = "The Tears of Eros",
  author = "Bataille, Georges" }
        
