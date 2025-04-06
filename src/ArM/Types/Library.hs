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
import Data.Text  (splitOn,unpack,pack)
import Text.Read 

import ArM.DB.CSV
import ArM.Types.TraitKey
import ArM.Char.Types.Advancement
import ArM.Helper
import ArM.Debug.Trace

-- | The stats of a book as required for advancement mechanics.
data BookStats = BookStats
         { topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
       }  deriving (Eq,Generic)
instance ToJSON BookStats
instance FromJSON BookStats
instance Show BookStats where
    show b = k ++ ' ':l ++ q
        where k = show $ topic b
              q = 'Q':(show $ quality b)
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
     , bookLanguage  :: Maybe String  -- ^ Language of the book
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

-- | The ID used to avoid rereading of tractatus
originalID :: Book -> String
originalID = bookID . originalBook

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

-- |
-- == CSV

readStats :: String -> (Maybe Int, Int)
readStats "" = trace "empty book stats" (Nothing, (-1))
readStats (' ':xs) = readStats xs
readStats ('Q':xs) = (Nothing, readMaybeInt xs)
readStats ('L':xs) = (Just $ readMaybeInt y, readMaybeInt z)
        where y:z:_ = map unpack $ splitOn "Q" $ pack xs
readStats x = trace ( "no parse: " ++ x ) (Nothing, (-1))

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
         } where (l,q) = readStats z


instance ArMCSV Book where
   fromCSVline (x0:x1:x2:x3':x4:x5:x6:x7:x8:_) =
      defaultObject { bookID = y 
                , bookTitle = x4
                , bookStats = [ makeBookStats x1 x2 x3 ]
                , bookCreator = x5
                , bookAnnotation = x6
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
     , copiedFrom = Nothing
     , bookLocation = ""
     , bookAnnotation = ""
     , bookLanguage = Nothing
     , bookCount = 1 }
   getID = bookID
