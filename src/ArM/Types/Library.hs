{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Library
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Types.Library where

import Data.Aeson
import Data.Aeson.Extra
import GHC.Generics
import Data.Maybe
import Data.Text  (splitOn,unpack,pack)
import Text.Read 
import Control.Monad

import ArM.DB.CSV
import ArM.Types.TraitKey
import ArM.Types
import ArM.Helper
import ArM.Debug.Trace

-- * Types

-- | The stats of a book as required for advancement mechanics.
data BookStats = BookStats
         { topic :: TraitKey
         , quality :: Maybe Int
         , bookLevel :: Maybe Int
         , reread :: Int
       }  deriving (Eq,Generic)
instance ToJSON BookStats
instance FromJSON BookStats where
    parseJSON = withObject "BookStats" $ \v -> BookStats
        <$> v .:? "topic" .!= NoTrait
        <*> v .:? "quality" 
        <*> v .:? "level" 
        <*> v .:? "reread"  .!= 1
instance Show BookStats where
    show b = k ++ ' ':l ++ q
        where k = show $ topic b
              q | isNothing (quality b) = ""
                | otherwise = 'Q':show (fromJust $ quality b)
              l | isNothing (bookLevel b) = ""
                | otherwise = 'L':show (fromJust $ bookLevel b)
instance Ord BookStats where
    compare a b | topic a /= topic b = compare (topic a) (topic b)
                | bookLevel a /= bookLevel b = compare (bookLevel a) (bookLevel b)
                | otherwise  = compare (quality a) (quality b)

-- | A book may be an original manuscript, an antology,  or a copy.
-- A copy will have a `copiedFrom` value, identifying the source.
-- An antology will have a non-empty `antologyOf` field, listing
-- all the constituent works.
--
-- A book may have one or more `BookStats` values.  An antology
-- should not have book stats, since the book stats are properties
-- of each constituent work.  A copy may or may not have book stats.
-- If it does not, it inherits stats from the original.
data Book = Book
     { bookID :: String
     , bookTitle :: String
     , bookStats :: [ BookStats ] -- ^ list of stats per topic covered
     , bookCreator :: String      -- ^ Creator of the copy or manuscript
     , bookDate :: SeasonTime     -- ^ Time the copy was made            
     , antologyOf :: [ Book ]     -- ^ The book is an antology of multiple books
     , copiedFrom :: Maybe String   -- ^ Book copied or Nothing for an original manuscript
     , bookLocation :: Maybe String     -- ^ Location whre the book was written or copied
     , bookNarrative :: [ String ]   -- ^ Additional information in free text
     , bookAnnotation :: [ String ]   -- ^ Additional information in free text
     , bookLanguage  :: Maybe String  -- ^ Language of the book
     , bookCount :: Int               -- ^ Number of copies 
     } deriving (Eq,Generic,Show)
instance Ord Book where
    compare a b | bookStats a /= bookStats b = compare (bookStats a) (bookStats b)
                | otherwise = compare (bookTitle a) (bookTitle b)
instance Countable Book where
    count = bookCount
    addCount b n = b { bookCount = bookCount b + n }
instance KeyObject Book where
   harmKey = BookKey . bookID
instance StoryObject Book where
    name book = tis ++ aus ++ dat
     where aut = trim $ originalAuthor book
           aus | aut == "" = ""
               | otherwise = " by " ++ aut
           tit = trim $ originalTitle book
           tis | tit == "" = ""
               | otherwise = "*" ++ tit ++ "*"
           dat = " (" ++ show (originalDate book) ++ ")"
    narrative = bookNarrative
    comment = bookAnnotation
instance ToJSON Book
instance FromJSON Book where
    parseJSON = withObject "Book" $ \v -> Book
        <$> v .:? "bookID" .!= "No ID"
        <*> v .:? "title" .!= "No title"
        <*> v `parseCollapsedList` "stats" 
        <*> v .:? "creator" .!= "N/A"
        <*> v .:? "date" .!= NoTime
        <*> v  `parseCollapsedList` "antologyOf" 
        <*> v .:? "copiedFrom" 
        <*> v .:? "location" 
        <*> v  `parseCollapsedList` "narrative" 
        <*> v  `parseCollapsedList` "comment" 
        <*> v .:? "language" 
        <*> v .:? "count"  .!= 1

-- * Convenience Functions
--
-- | Is the book a tractatus or something else?
isTractatus :: Book -> Bool
isTractatus = f . bookStats 
    where f [] = False
          f (x:_) = isJust ( quality x ) && isNothing ( bookLevel x )

{-
-- | The original of a given book (constituent book in the case of an anotology)
originalBook :: Book -> Maybe HarmKey -> Maybe Book
originalBook b Nothing = Just $ originalTome b
originalBook b (Just k) = fmap originalTome $ find ( (==k) . harmKey ) $ antologyOf b
-}

-- | The `BookDB` class is any type wherein one may look up books by
-- their ID.
class BookDB h where
   -- | Look up a book by key (String) in a database.
   bookLookup :: h -> String -> Maybe Book
   bookLookup db k = lookupBook k db 
   -- | Look up a book by key (String) in a database.
   -- This is equivalent to `bookLookup` with the arguments swapped
   lookupBook :: String -> h -> Maybe Book
   lookupBook k db = bookLookup db k

instance (BookDB h) => BookDB [h] where
   lookupBook k = foldl mplus Nothing . map (\ x -> bookLookup x k) 
instance BookDB Book where
   bookLookup bk k | k == bookID bk = Just bk
                   | otherwise = Nothing

-- | The ID used to avoid rereading of tractatus
originalKey :: BookDB h => h -> Book -> HarmKey
originalKey db = harmKey . originalTome db

-- | The ID used to avoid rereading of tractatus
originalID :: BookDB h => h -> Book -> String
originalID db = bookID . originalTome db

-- | The original of a given tome
originalTome :: BookDB h => h -> Book -> Book
originalTome db b
   | isNothing b' = b
   | otherwise = originalTome db (fromJust b')
   where b' = join $ fmap (bookLookup db) (copiedFrom b)

{-
bookTraitStats :: Book -> TraitKey -> Maybe BookStats
bookTraitStats b k = find ( (==k) . topic ) $ bookStats b
-}

-- | The original author of a given book
originalTitle :: Book -> String
originalTitle = bookTitle 
-- | The original author of a given book
originalAuthor :: Book -> String
originalAuthor = bookCreator 
-- | The original date the book was authored
originalDate :: Book -> SeasonTime
originalDate = bookDate 

-- | Get the unique identifier of an original book
bookKey :: Book -> HarmKey
bookKey = BookKey . bookID


-- * CSV

readStats :: String -> (Maybe Int, Maybe Int)
readStats "" = trace "empty book stats" (Nothing, Nothing)
readStats "Spell" = trace "empty book stats" (Nothing, Nothing)
readStats (' ':xs) = readStats xs
readStats ('Q':xs) = (Nothing, Just $ readMaybeInt xs)
readStats ('L':xs) = (lvl ys, ql ys)
        where ys = map ( readMaybeInt . unpack ) $ splitOn "Q" $ pack xs
              lvl = maybeHead 
              ql (_:x:_) = Just x
              ql _ = Nothing
readStats x = trace ( "no parse: " ++ x ) (Nothing, Nothing)

readMaybeInt :: String -> Int
readMaybeInt = fromMaybe (-1) . readMaybe

readTopic :: String -> String -> TraitKey
readTopic x = readTopic' (trim x)
readTopic' :: String -> String -> TraitKey
readTopic' "Art" y  = ArtKey $ trim y
readTopic' "Ability" y  = AbilityKey $ trim y
-- readTopic' "Spell" y  = SpellKey $ trim y
readTopic' _ y  = AbilityKey $ trim y

makeBookStats :: String -> String -> String -> BookStats
makeBookStats x y z = BookStats 
         { topic = readTopic x y
         , quality = q
         , bookLevel = l
         , reread = 1
         } where (l,q) = readStats z


instance ArMCSV Book where
   fromCSVline (x0:x1:x2:x3':x4:x5:x6:x7:x8:_) =
      defaultObject { bookID = y 
                , bookTitle = x4
                , bookStats = [ makeBookStats x1 x2 x3 ]
                , bookCreator = x5
                , bookAnnotation = [x6]
                , bookCount = fromMaybe 1 $ readMaybe x7
                , bookLanguage = lng
                }
                where y | xid == "" = trim x2 ++ x3 ++ " " ++ trim x4
                        | otherwise = xid
                      xid = trim x0
                      x3 = trim x3'
                      lng' = trim x8
                      lng | lng' == "" = Nothing
                          | otherwise = Just lng'
   fromCSVline _ = defaultObject
   defaultObject = Book
     { bookID = ""
     , bookTitle = ""
     , bookStats = [ ] 
     , bookCreator = ""
     , bookDate = NoTime
     , antologyOf = []
     , copiedFrom = Nothing
     , bookLocation = Nothing
     , bookNarrative = []
     , bookAnnotation = []
     , bookLanguage = Nothing
     , bookCount = 1 }
   getID = bookID


-- * Descriptions of a reading season

-- | Currently unused, this is one idea for describing what book and part
-- is read in a given season.
data ReadingID = ReadingID
     { bookRead :: HarmKey
     , partRead :: Maybe HarmKey
     , topicRead :: TraitKey
     } deriving (Eq,Show,Generic)
instance ToJSON ReadingID
instance FromJSON ReadingID {- where
    parseJSON = withObject "ReadingID" $ \v -> ReadingID
                    <$> v .:  "tome" 
                    <*> v .:? "part"
                    <*> fmap AbilityKey ( v .:? "ability" )
                    <*> fmap ArtKey ( v .:? "art" )
-}

