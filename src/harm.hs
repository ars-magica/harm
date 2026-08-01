-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2023: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  
--    harm is a command-line interface to character generation for Ars Magica.
--
-- Characters and their advancement are specified in a JSON or YAML format,
-- which is validated and converted to markdown file, showing the state of the
-- characters at different points in time.
--
-- Covenant generation and advancement is also covered.
--
-- This is work in progress, and may features are missing.
--
-- == Implementation
--
-- The `main` function essentially only calls two functions from `ArM.IO`.
-- First `readSaga` to load the saga file and all its constituent files,
-- and the `writeSaga` to process all the advancements and write the resulting
-- character sheets.
--
-- See `ArM.IO` for a description of these two files.
--
-----------------------------------------------------------------------------

module Main (main) where

import System.Environment
import System.Console.GetOpt

import ArM.Helper (putStrLns)
import ArM.Debug.Time
import ArM.IO

import Data.Maybe (fromJust)

-- import ArM.Debug.Trace

data Options = Options 
  { sagaFile :: Maybe String
  , spellDBFile :: Maybe String
} deriving (Show)
defaultOptions :: Options
defaultOptions = Options 
  { sagaFile = Nothing
  , spellDBFile = Nothing
}


options :: [ OptDescr (Options -> Options) ]
options =
    [ Option ['s']     ["saga"] (ReqArg 
            (\arg opt -> opt { sagaFile = Just arg })
            "FILE") "saga file"
    , Option ['S']     ["spells"] (ReqArg 
            (\arg opt -> opt { spellDBFile = Just arg })
            "FILE") "input file for spell database"
    ]

armcharOpts :: [String] -> IO (Options, [String])
armcharOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: armchar [OPTION...] "

-- | The program will read the given saga file and constituent files and
-- generate all the character and covenant sheets requested by the saga file.
main :: IO ()
main = do 
     putStrLn "Starting: harm ..."
     putStrLn "Testing 1 Aug 2026"
     printTime
     args <- getArgs
     (opt,n) <- armcharOpts args
     putStrLns n
     -- putStrLn $ "Options: " ++ show opt

     main' opt

main' :: Options -> IO ()
main' opts | sagaFile opts /= Nothing = do 
     saga <- readSaga $ fromJust $ sagaFile opts
     case saga of
        Nothing -> error "Could not read Saga file"
        (Just s1) -> do
               writeSaga s1
               return ()
main' _ | otherwise = error "Not implemented!" 
