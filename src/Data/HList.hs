-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OList
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Hierarchically organised text.
--
-- The `OList` type contains nested lists of strings, which can easily
-- be output as nested bullet lists, or as sections and subsection,
-- in markdown or other markup languages.
--
-----------------------------------------------------------------------------
module Data.OList ( HList(..)
                  , headHList
                  , toHList
                  , indentHList
       ) where

import Data.Char
import Data.OList
import System.IO as IO -- for file IO

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
toHList :: [ String ] -> OList
toHList [] = HList "" []
toHList (x:xs) = HList x $ map tHList xs

toHList' :: String -> HList
toHList' s = HList s []

-- | Render an OList as a hierarchical markdown list
indentOList :: HList -> OList
indentOList = indentOList' "+ "

indentOList' :: String -> HList -> OList
indentOList' s (HList x ys) = HList x' ys'
     where x' = s ++ x
           ys' = map (indentOList' ("    "++s)) ys

