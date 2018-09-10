{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

main :: IO ()
main = TIO.writeFile "books.html" (booksToHtml myBooks)
