{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( processRecords
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


getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where recordLength = getRecordLength record
        baseAddress = getBaseAddress record
        baseRecord = BS.drop baseAddress record
        baseAtEntry = BS.drop (fieldStart fieldMetadata) baseRecord
        byteStringValue = BS.take (fieldLength fieldMetadata) baseAtEntry


fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'


lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if length results < 1
                                  then Nothing
                                  else Just (head results)
  where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
        results = filter ((== aTag) . tag) metadata


lookupSubfield :: (Maybe FieldMetadata) -> Char ->
                  MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
    if results == []
    then Nothing
    else Just ((T.drop 1 . head) results)
  where rawField = getTextField record fieldMetadata
        subfields = T.split (== fieldDelimiter) rawField
        results = filter ((== subfield) . T.head) subfields


lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata
                                                  subfield record
  where entryMetadata = lookupFieldMetadata aTag record


lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield


marcToPairs :: BS.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where records = allRecords marcStream
        titles = map lookupTitle records
        authors = map lookupAuthor records


pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
                             title = fromJust title,
                             author = fromJust author}) justPairs
  where justPairs = filter (\(title,author) -> isJust title
                                               && isJust author) pairs


processRecords :: Int -> BS.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs
