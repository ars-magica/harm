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

-- |
-- Trim away whitespace from the head and tail of the string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- |
-- Division of integers, rounding up
(//) :: Integral a => a -> a -> a
(//) x y = (x+1) `div` y

-- |
-- Remove elements from the second list from the first.
-- Both lists have to be sorted.
(-=) :: Ord a => [a] -> [a] -> [a] 
(-=) [] _ = []
(-=) xs [] = xs
(-=) (x:xs) (y:ys) | x < y = x:(xs -= (y:ys))
                   | x > y = (x:xs) -= ys
                   | otherwise = xs -= ys

-- * Convenience functions for Maybe

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


-- | Skip Nothing values and apply the give function on Just-objects.
passMaybe :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
passMaybe _ Nothing = return Nothing
passMaybe g (Just x) = fmap Just $ g x

-- | Show a Maybe value, with an empty string for Nothing
maybeShow :: Show a => Maybe a -> String
maybeShow = fromMaybe "" . fmap show

-- * Rendering Numbers

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

-- * List Management

-- | Count duplications in a list of books.
countRepetitions :: Eq b
           => [b]       -- ^ Sorted list of books with duplications
           -> [(b,Int)] -- ^ List of unique books with number of repetitions in the input
countRepetitions = f . map ( \b -> (b,1) ) 
   where f [] = []
         f (x:[]) = x:[]
         f (x:y:ys) | fst x /= fst y  = x:f (y:ys)
                    | otherwise = f ((fst x,snd x + snd y):ys)

-- | Remove Nothing elements from a list, and map the Just-elements
-- to the constituent object.
filterNothing :: [Maybe a] -> [a]
filterNothing = f
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs

-- | Sort the list and remove duplicates.
uniqueSort :: (Ord a,Eq a) => [a] -> [a]
uniqueSort = f . sort
    where f [] = []
          f (x:[]) = x:[]
          f (x:y:ys) | x == y = f (y:ys)
                     | otherwise = x:f (y:ys)

-- * Convenience functions for Markdown

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

-- * Plain Text Rendering

-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of `Show`.
commaList :: Show a => [a] -> String
commaList = showStrList . map show

-- | Show a list of strings without brackets and quotes.
showStrList :: [String] -> String
showStrList [] = ""
showStrList (x:xs) = foldl (++) x $ map (", "++) xs

-- | Show a string but not an empty one
nonemptyStringMD :: String -> OList
nonemptyStringMD "" = OList [] 
nonemptyStringMD st = OString st

-- | Render a Maybe String as an OList.
-- Nothing becomes an empty OList and a Just object becomes a single line.
-- Note that this is different from the generic instance for Maybe, because
-- of the difficulties making an instance for String.
stringMD :: Maybe String -> OList
stringMD Nothing = OList []
stringMD (Just x) = OString x

