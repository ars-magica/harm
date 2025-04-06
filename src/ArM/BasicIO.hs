-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
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
module ArM.BasicIO where

import System.IO as IO -- for file IO
import Data.Aeson
import Data.Aeson.Text
import Data.Text.Lazy.IO as I

-- | 
-- = The OList type

-- | Nested lists of strings.
-- This is intended to build output files, where each atomic object is rendered 
-- as a list of lines and composite objects as a list of rendered constituent
-- objects.
data OList = OList [ OList ] | OString String deriving ( Show )


-- | Convert a list of Strings to a OList object
toOList :: [ String ] -> OList
toOList= OList . map OString 

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

-- | Render an OList as a hierarchical markdown list
indentOList1 :: OList -> OList
indentOList1 (OString x) = OString $ '+':' ':x
indentOList1 (OList xs) = OList $ map (indentOList' "+ ") xs

indentOList1' :: String -> OList -> OList
indentOList1' s (OString x) = OString $ s ++ x
indentOList1' s (OList xs) = OList $ map (indentOList' ("    "++s)) xs

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

writeMaybeOList :: Maybe String -> OList -> IO ()
writeMaybeOList Nothing   = \ _ -> return ()
writeMaybeOList (Just fn) = writeOList fn

-- | 
-- = File output

-- | Write a list of strings into the given file
writeLns :: String     -- ^ File name
           -> [ String ] -- ^ Contents
           -> IO ()
writeLns fn c = do
     handle <- openFile fn WriteMode
     let p = IO.hPutStrLn handle
     mapM_ p c
     hClose handle

writeMaybeFile :: Maybe String     -- ^ File name
           -> [ String ]           -- ^ Contents
           -> IO ()
writeMaybeFile Nothing _ = return ()
writeMaybeFile (Just x) y =  writeLns x y

writeMaybeJSON :: ToJSON t => Maybe String -> t -> IO ()
writeMaybeJSON Nothing _ = return ()
writeMaybeJSON (Just fn) y' = I.writeFile fn y
   where y = encodeToLazyText y'

putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = IO.putStrLn x >> putStrLns xs
