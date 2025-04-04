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

data BookOriginal = BookOriginal
         { bookTitle :: String
         , bookStats :: BookStats
         , bookAuthor :: String
         , bookYear :: Int
         , bookLocation :: String
         , bookAnnotation :: String
       }  deriving (Eq,Generic,Show)
instance ToJSON BookOriginal
instance FromJSON BookOriginal

data BookKey = BookKey String
   deriving (Show,Eq,Generic)
instance ToJSON BookKey
instance FromJSON BookKey
bookKey :: BookOriginal -> BookKey
bookKey b = BookKey $ show (bookStats b) ++ ":" ++ (bookTitle b)

data BookCopy = BookCopy
         { copyTitle :: String
         , copyStats :: BookStats
         , copyist :: String
         , copyYear :: Int
         , copyLocation :: String
         , copyAnnotation :: String
         , original :: BookKey
       }  deriving (Eq,Generic,Show)
instance ToJSON BookCopy
instance FromJSON BookCopy

data BookStats = BookStats
         { topic :: TraitKey
         , quality :: Int
         , bookLevel :: Maybe Int
       }  deriving (Eq,Generic)
instance ToJSON BookStats
instance FromJSON BookStats

instance Ord BookOriginal where
    compare a b | bookStats a /= bookStats b = compare (bookStats a) (bookStats b)
                | otherwise = compare (bookTitle a) (bookTitle b)
instance Ord BookCopy where
    compare a b | copyStats a /= copyStats b = compare (copyStats a) (copyStats b)
                | otherwise  = compare (copyTitle a) (copyTitle b)
instance Ord BookStats where
    compare a b | topic a /= topic b = compare (topic a) (topic b)
                | bookLevel a /= bookLevel b = compare (bookLevel a) (bookLevel b)
                | otherwise  = compare (quality a) (quality b)
instance Show BookStats where
    show b = k ++ l ++ q
        where k = show $ topic b
              q = show $ quality b
              l | isNothing (bookLevel b) = ""
                | otherwise = 'L':show (fromJust $ bookLevel b)

