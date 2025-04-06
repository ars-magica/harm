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
     { bookTitle :: String
     , bookStats :: [ BookStats ]
     , bookCreator :: String      -- ^ Creator of the copy or manuscript
     , bookDate :: SeasonTime     -- ^ Time the copy was made            
     , copiedFrom :: Maybe Book   -- ^ Book copied or Nothing for an original manuscript
     , bookLocation :: String     -- ^ Location whre the book was written or copied
     , bookAnnotation :: String
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

-- | The original author of a given book
originalTitle :: Book -> String
originalTitle = bookTitle . originalBook

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

