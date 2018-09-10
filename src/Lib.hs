{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( booksToHtml
    ) where

import Lib.Types
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe


bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n",
                           titleInTags,
                           authorInTags,
                           "</p>\n"]
  where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
        authorInTags = mconcat ["<em>", (author book), "</em>\n"]


booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n",
                             "<head><title>books</title>",
                             "<meta charset='utf-8'/>",
                             "</head>\n",
                             "<body>\n",
                             booksHtml,
                             "\n</body>\n",
                             "</html>"]
  where booksHtml = (mconcat . (map bookToHtml)) books
                             

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: BS.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: BS.ByteString -> (MarcRecordRaw,B.ByteString)
nextAndRest marcStream = BS.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

allRecords :: BS.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == BS.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = nextAndRest marcStream
