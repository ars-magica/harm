{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Helper
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Generic and simple helper functions
--
--
-----------------------------------------------------------------------------
module ArM.Helper where

import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Network.URI.Encode as URI
import ArM.BasicIO
import Data.Char
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Extra
import GHC.Generics
import qualified Data.Text as T
import ArM.Debug.Trace

-- | Parse an optional list of strings from JSON, accepting a string
-- as a singleton list.
parseSingleton :: Object -> T.Text -> Parser [String]
parseSingleton v k = fmap singleton ( v `parseCollapsedList` k )

newtype StringList = StringList [String] deriving (Generic,Show,Eq)
instance FromJSON StringList

singleton :: Maybe StringList -> [String]
singleton Nothing = ttrace []
singleton (Just (StringList xs)) = ttrace xs

-- |
-- Trim away whitespace from the head and tail of the string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- |
-- Remove elements from the second list from the first.
-- Both lists have to be sorted.
(-=) :: Ord a => [a] -> [a] -> [a] 
(-=) [] _ = []
(-=) xs [] = xs
(-=) (x:xs) (y:ys) | x < y = x:(xs -= (y:ys))
                   | x > y = (x:xs) -= ys
                   | otherwise = xs -= ys

-- |
-- = Convenience functions for Maybe

{-
-- | Collapse a neste Maybe Maybe object to a single Maybe
join :: Maybe (Maybe a) -> Maybe a
join Nothing = Nothing
join (Just Nothing) = Nothing
join (Just (Just x)) = Just x
-}

-- | Show a number or «N/A»
showstat :: Show a => Maybe a -> String
showstat Nothing = "N/A"
showstat (Just x) = show x

-- | return the head of a list or Nothing if the list is empty
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- | Get a list from a maybe-list, mapping Nothing to the empty list.
maybeList :: Maybe [a] -> [a]
maybeList = fromMaybe []

-- | Add maybe-integers, treating Nothing as zero.
maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd Nothing Nothing = Nothing 
maybeAdd x y = Just $ fromMaybe 0 x + fromMaybe 0 y

-- | Remove Nothing elements from a list, and map the Just-elements
-- to the constituent object.
filterNothing :: [Maybe a] -> [a]
filterNothing = f
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

-- | Skip Nothing values and apply the give function on Just-objects.
passMaybe :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
passMaybe _ Nothing = return Nothing
passMaybe g (Just x) = fmap Just $ g x

-- |
-- = Convenience display functions 

maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = ""
maybeShow (Just x) = show x

-- | Show a number with decimals only if required.
showNum :: (Show a, RealFrac a) => a -> String
showNum x | isInt x = show ( round x :: Int )
          | otherwise = show x
    where isInt i = i == fromInteger (round i)

-- | Show a non-zero integer with sign, or an empty string
showBonus :: Int -> String
showBonus x | x > 0 = " +" ++ show x
            | x < 0 = " " ++ show x
            | otherwise = ""

-- | Show an integer with sign
showSigned :: Int -> String
showSigned x | x > 0 = "+" ++ show x
            | otherwise = show x

-- | Show a list of strings without brackets and quotes.
showStrList :: [String] -> String
showStrList [] = ""
showStrList (x:xs) = foldl (++) x $ map (", "++) xs

-- |
-- = Unique Sort

-- | Sort the list and remove duplicates.
uniqueSort :: (Ord a,Eq a) => [a] -> [a]
uniqueSort = f . sort
    where f [] = []
          f (x:[]) = x:[]
          f (x:y:ys) | x == y = f (y:ys)
                     | otherwise = x:f (y:ys)

-- |
-- = Convenience functions for Markdown

-- | Set a markdown link, escaping spaces in the link.
markdownLink :: String -> String -> String
markdownLink txt lnk = "[" ++ txt ++ "](" ++ URI.encode lnk ++ ")"

-- | Set a link for github pages
pagesLink :: String -> String
pagesLink lnk = "[" ++ lnk ++ "](" ++ lnk ++ ")"

-- | Set a wikilink
wikiLink :: String -> String
wikiLink txt = "[[" ++ txt ++ "]]"

-- | Set an item for a description list in markdown 
markdownDL :: String -> String -> OList
markdownDL t d = OList [ OString t, OString (": "++d), OString "" ]

