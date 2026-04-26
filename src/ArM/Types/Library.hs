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
module ArM.Types.Library ( BookStats(..)
                         , Book(..)
                         , BookDB(..)
                         , readBookCSV
                         , isTractatus
                         , bookKey
                         ) where

import Data.Aeson
import Data.Aeson.Extra
import GHC.Generics
import Data.Maybe
import Data.Text  (splitOn,unpack,pack)
import Text.Read 
import Control.Monad

import ArM.Types.Trait
import ArM.Helper
import ArM.Debug.Trace
import qualified ArM.Internal.Book as IB

-- * Types

-- | The stats of a book as required for advancement mechanics.
data BookStats = BookStats
         { topic :: TraitKey
         , quality :: Maybe Int
         , bookLevel :: Maybe Int
         , reread :: Int          
            -- ^ Number of tractatus in the text.  This is normally 1
            -- and ignored for any text but tractatus, but there are a
            -- few canon examples of texts that count as multiple tractatus.
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

-- | A book is an original manuscript.  Antologies and copies are
-- handled as Possession objects.
--
-- A book may have one or more `BookStat` values.  A copy may or may
-- not have book stats.  If it does not, it inherits stats from the original.
data Book = Book
     { bookID :: String
     , bookTitle :: String
     , bookStats :: [ BookStats ] -- ^ list of stats per topic covered
     , bookAuthor :: String      -- ^ Creator of the copy or manuscript
     , bookDate :: SeasonTime     -- ^ Time the copy was made            
     , bookLocation :: Maybe String     -- ^ Location where the book was written or copied
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
     where aut = trim $ bookAuthor book
           aus | aut == "" = ""
               | otherwise = " by " ++ aut
           tit = trim $ bookTitle book
           tis | tit == "" = ""
               | otherwise = "*" ++ tit ++ "*"
           dat = " (" ++ show (bookDate book) ++ ")"
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


makeBookStats :: String   -- ^ trait type
              -> String   -- ^ trait label
              -> String   -- ^ Stat String
              -> BookStats -- ^ Book stat object
makeBookStats x y z = BookStats 
         { topic = readTopic x y
         , quality = q
         , bookLevel = l
         , reread = 1
         } where (l,q) = readStats z

-- | A default book object, providing defaults for fields not available in the CSV format.
defaultBook :: Book
defaultBook = Book
     { bookID = ""
     , bookTitle = ""
     , bookStats = [ ] 
     , bookAuthor = ""
     , bookDate = NoTime
     , bookLocation = Nothing
     , bookNarrative = []
     , bookAnnotation = []
     , bookLanguage = Nothing
     , bookCount = 1 }

-- | Translate from the `RawBook` format used by the CSV parser to 
-- the `Book` format.
fromRawBook :: IB.RawBook -> Book
fromRawBook rb = 
      defaultBook { bookID = IB.key  rb
                , bookTitle = IB.title rb
                , bookStats = [ makeBookStats (IB.traittype rb) (IB.trait rb) (IB.stats rb) ]
                , bookAuthor = IB.creator rb
                , bookAnnotation = [ IB.comment rb ]
                , bookCount = IB.copies rb
                , bookLanguage = Just $ IB.language rb
                }

-- | Parse the given file and return a list of books.
readBookCSV :: String -> IO [Book]
readBookCSV fn = IB.readBookCSV fn >>= return . map fromRawBook

-- * Descriptions of a reading season

-- | Currently unused, this is one idea for describing what book and part
-- is read in a given season.
data ReadingID = ReadingID
     { bookRead :: HarmKey
     , partRead :: Maybe HarmKey
     , topicRead :: TraitKey
     } deriving (Eq,Show,Generic)
instance ToJSON ReadingID
instance FromJSON ReadingID 

