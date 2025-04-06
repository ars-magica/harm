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
import GHC.Generics
import Data.Maybe

import ArM.DB.CSV
import ArM.Types.TraitKey
import ArM.Char.Types.Advancement

-- | The stats of a book as required for advancement mechanics.
data BookStats = BookStats
         { topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
       }  deriving (Eq,Generic)
instance ToJSON BookStats
instance FromJSON BookStats
instance Show BookStats where
    show b = k ++ l ++ q
        where k = show $ topic b
              q = show $ quality b
              l | isNothing (bookLevel b) = ""
                | otherwise = 'L':show (fromJust $ bookLevel b)

-- | A book may be an original manuscript or a copy.
data Book = Book
     { bookID :: String
     , bookTitle :: String
     , bookStats :: [ BookStats ] -- ^ list of stats per topic covered
     , bookCreator :: String      -- ^ Creator of the copy or manuscript
     , bookDate :: SeasonTime     -- ^ Time the copy was made            
     , copiedFrom :: Maybe Book   -- ^ Book copied or Nothing for an original manuscript
     , bookLocation :: String     -- ^ Location whre the book was written or copied
     , bookAnnotation :: String   -- ^ Additional information in free text
     , bookCount :: Int               -- ^ Number of copies 
     } deriving (Eq,Generic,Show)
instance ToJSON Book
instance FromJSON Book

-- | Type for the unique identifier of an original book
data BookKey = BookKey String
   deriving (Show,Eq,Generic)

instance ToJSON BookKey
instance FromJSON BookKey

-- | The original of a given book
originalBook :: Book -> Book
originalBook b
   | isNothing (copiedFrom b) = b
   | otherwise = originalBook (fromJust $ copiedFrom b)

-- | The original date the book was authored
originalDate :: Book -> SeasonTime
originalDate = bookDate . originalBook

-- | The original author of a given book
originalAuthor :: Book -> String
originalAuthor = bookCreator . originalBook

-- | Get the unique identifier of an original book
bookKey :: Book -> BookKey
bookKey b = BookKey $ show (bookStats b) ++ ":" ++ (bookTitle b)

-- |
-- = Other instances

instance Ord Book where
    compare a b | bookStats a /= bookStats b = compare (bookStats a) (bookStats b)
                | otherwise = compare (bookTitle a) (bookTitle b)
instance Ord BookStats where
    compare a b | topic a /= topic b = compare (topic a) (topic b)
                | bookLevel a /= bookLevel b = compare (bookLevel a) (bookLevel b)
                | otherwise  = compare (quality a) (quality b)


instance ArMCSV SpellRecord where
   fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:_) =
      defaultObject { bookID = x1 
                , bookTitle = x2
                , bookStats = [ makeBookStats x3 x4 x5 x6 ]
                , bookCreator = x7
                , bookDate = x8
                , bookTime = x9
                , bookLocation = x10
                , bookAnnotation = x11
                , bookCount = x12
                }
   fromCSVline _ = defaultObject
   defaultObject = Book
     { bookID = ""
     , bookTitle = ""
     , bookStats = [ ] 
     , bookCreator = ""
     , bookDate = NoTime
     , copiedFrom = Nothing
     , bookLocation = ""
     , bookAnnotation = ""
     , bookCount = 1 }
   getID = spellRecordName
