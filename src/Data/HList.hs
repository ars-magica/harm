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
                  , headHList
                  , toHList
                  , indentHList
       ) where

import Data.OList

-- | Nested lists of strings.
-- This is intended to build output files, where each atomic object is rendered 
-- as a list of lines and composite objects as a list of rendered constituent
-- objects.
--
-- Hierarchical bullet points can be made from a vanilla OList with the
-- `indentOList` function.
data HList = HList String [ HList ] 

headHList :: String -> [ String ] -> OList
headHList _ [] = OList []
headHList s xs = OList [ OString s, OList $ map OString xs ]

-- | Convert a list of Strings to a OList object
toHList :: [ String ] -> HList
toHList [] = HList "" []
toHList (x:xs) = HList x $ map toHList' xs

toHList' :: String -> HList
toHList' s = HList s []

fromHList :: HList -> OList
fromHList (HList x []) = OString x
fromHList (HList x ys) = OList $ OString x:OList (map fromHList ys):[]


-- | Render an OList as a hierarchical markdown list
indentHList :: HList -> OList
indentHList = fromHList . indentHList' "+ "

indentHList' :: String -> HList -> HList
indentHList' s (HList x ys) = HList x' ys'
     where x' = s ++ x
           ys' = map (indentHList' ("    "++s)) ys

