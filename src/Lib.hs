{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( booksToHtml
    , allRecords
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
getLeader record = BS.take leaderLength record

rawToInt :: BS.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (BS.take 5 leader)

nextAndRest :: BS.ByteString -> (MarcRecordRaw,BS.ByteString)
nextAndRest marcStream = BS.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

allRecords :: BS.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == BS.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = nextAndRest marcStream


getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (BS.take 5 remainder)
  where remainder = BS.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = BS.take directoryLength afterLeader
  where directoryLength = getDirectoryLength record
        afterLeader = BS.drop leaderLength record


dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == BS.empty
                           then []
                           else nextEntry : splitDirectory restEntries
  where (nextEntry, restEntries) = BS.splitAt dirEntryLength directory


makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where (theTag,rest) = BS.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength,rawStart) = BS.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart = rawToInt rawStart


getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries


