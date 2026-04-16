-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.BasicIO
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
module ArM.BasicIO ( putStrLns) where

import System.IO as IO -- for file IO

putStrLns :: [ String ] -> IO ()
putStrLns [] = return ()
putStrLns (x:xs) = IO.putStrLn x >> putStrLns xs
