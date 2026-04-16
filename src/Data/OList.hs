-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OList
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities to read and write files.
--
-- This module contains convenience functions for file output.
--
-----------------------------------------------------------------------------
module Data.OList ( OList(..)
                  , headOList
                  , toOList
                  , foldOList
                  , indentOList
                  , writeOListH
                  , writeOList
       ) where

import System.IO as IO -- for file IO

-- | 
-- = The OList type

-- | Nested lists of strings.
-- This is intended to build output files, where each atomic object is rendered 
-- as a list of lines and composite objects as a list of rendered constituent
-- objects.
--
-- Hierarchical bullet points can be made from a vanilla OList with the
-- `indentOList` function.
data OList = OList [ OList ] | OString String deriving ( Show )

headOList :: String -> [ String ] -> OList
headOList _ [] = OList []
headOList s xs = OList [ OString s, OList $ map OString xs ]

-- | Convert a list of Strings to a OList object
toOList :: [ String ] -> OList
toOList = OList . map OString 

-- | Fold the first layer of a nested OList
foldOList :: OList -> OList
foldOList (OString x) = OString x
foldOList (OList x) = OList $ f x
   where f [] = []
         f (OList y:ys) = y ++ f ys
         f (OString y:ys) = OString y:f ys


-- | Render an OList as a hierarchical markdown list
indentOList :: OList -> OList
indentOList (OString x) = OString $ '+':' ':x
indentOList (OList xs) = OList $ map (indentOList' "+ ") xs

indentOList' :: String -> OList -> OList
indentOList' s (OString x) = OString $ s ++ x
indentOList' s (OList xs) = OList $ map (indentOList' ("    "++s)) xs

-- | 
-- == Writing OList to file

writeOListH :: Handle -> OList -> IO ()
writeOListH h (OString x) = IO.hPutStrLn h x
writeOListH _ (OList []) = return ()
writeOListH h (OList (x:xs)) = writeOListH h x  >> writeOListH h (OList xs)

writeOList :: String -> OList -> IO ()
writeOList fn x = do
     handle <- openFile fn WriteMode
     writeOListH handle x
     hClose handle

