-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HList
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Hierarchically organised text.
--
-- The `HList` type contains nested lists of strings, which can easily
-- be output as nested bullet lists, or as sections and subsection,
-- in markdown or other markup languages.
--
-----------------------------------------------------------------------------
module Data.HList ( HList(..)
                  , toHList
                  , hlist
                  , maybeHList
                  , fromHList
                  , indentHList
                  , isEmptyHList 
                  , pushTitle
                  , appendToHList
                  , italicHString
       ) where

import Data.Char
import Data.OList
import ArM.Debug.Trace

-- | Nested lists of strings.
-- This is intended to build output files, where each atomic object is rendered 
-- as a list of lines and composite objects as a list of rendered constituent
-- objects.
--
-- Hierarchical bullet points can be made from a vanilla OList with the
-- `indentOList` function.
data HList = HList String [ HList ] 
   deriving (Show)

-- | Convert a list of Strings to a OList object
toHList :: [ String ] -> HList
toHList [] = HList "" []
toHList ("":xs) = toHList xs
toHList (x:xs) = HList (trace ("[toHList] "++x) x) $ map hlist xs

hlist :: String -> HList
hlist s = HList s []

pushTitle :: String -> HList -> HList
pushTitle s (HList x xs) = HList s (HList x []:xs)

maybeHList :: String -> Maybe HList
maybeHList "" = Nothing
maybeHList s = Just $ HList s []

appendToHList :: [ HList ] -> HList -> HList
appendToHList ys (HList s xs) = HList s (xs++ys)



-- | Convert a 'HList' to an 'OList'.
-- This aims to make `fromHList . indentHList` and `indentOList . fromHList`
-- equivalent.
fromHList :: HList -> OList
fromHList (HList "" []) = OList []
fromHList (HList x []) = OString x
fromHList (HList x ys) = OList $ OString x:(OList $ fromHHList ys):[]

-- | Convert a list of 'HList' to a list of 'OList'.
-- This is an auxiliary for `fromHList`.
fromHHList :: [HList] -> [OList]
fromHHList [] = []
fromHHList (HList x xs:hs) = OString x:OList (fromHHList xs):fromHHList hs


-- | Is the `HList` empty?
-- It is considered empty if both the header string and the
-- list of subsidiaries are empty.
isEmptyHList :: HList -> Bool
isEmptyHList (HList "" []) = True
isEmptyHList _ = False


-- | Render an OList as a hierarchical markdown list
indentHList :: HList -> OList
indentHList = fromHList . indentHList' "+ "

indentHList' :: String -> HList -> HList
indentHList' s (HList x ys) = HList x' ys'
     where x' = s ++ x
           ys' = map (indentHList' ("    "++s)) ys

{-
class ToOList a where
   -- | Convert to an OList object
   toOList :: a -> OList
instance ToOList a => ToOList [a] where
   toOList = OList . map toOList 
instance ToOList HList where
   toOList = fromHList
instance ToOList (Maybe HList) where
   toOList = fromHList . filterNothing 
-}

-- | Render a string in italics, as an HList
italicHString :: String  -> HList
italicHString c = hlist $ "*" ++ (f . f) c ++ "*"
   where f = reverse . dropWhile isSpace
