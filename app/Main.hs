{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Lib.Types
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe


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

myBooks :: [Book]
myBooks = [book1,book2,book3]


main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let marcRecords = allRecords marcData
  print (length marcRecords)
