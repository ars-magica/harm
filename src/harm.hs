-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2023: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------

module Main where

import System.Environment
import System.Console.GetOpt

import ArM.BasicIO
import ArM.Debug.Time
import ArM.IO
import ArM.Char.Character
import ArM.Markdown


import Data.Maybe (fromJust)

-- import ArM.Debug.Trace

data Options = Options 
  { sagaFile :: Maybe String
  , charFile :: Maybe String
  , outFile  :: Maybe String
  , jsonFile :: Maybe String
  , spellDBFile :: Maybe String
  , debugFile  :: Maybe String
  , seasonFile  :: Maybe String
  , advanceSeason  :: Maybe String
} deriving (Show)
defaultOptions :: Options
defaultOptions = Options 
  { sagaFile = Just "Data/hibernia.json"
  , charFile = Nothing
  , outFile  = Nothing
  , jsonFile  = Nothing
  , spellDBFile = Nothing
  , debugFile  = Nothing
  , seasonFile  = Nothing
  , advanceSeason  = Nothing
}


options :: [ OptDescr (Options -> Options) ]
options =
    [ Option ['c']     ["character"] (ReqArg 
            (\arg opt -> opt { charFile = Just arg })
            "FILE") "character file"
    , Option ['j']     ["json"] (ReqArg 
            (\arg opt -> opt { jsonFile = Just arg })
            "FILE") "JSON output file"
    , Option ['o']     ["output"]  (ReqArg 
            (\arg opt -> opt { outFile = Just arg })
            "FILE") "output file"
    , Option ['O']     ["debug-output"] (ReqArg 
            (\arg opt -> opt { debugFile = Just arg })
            "FILE") "debug output"
    , Option ['s']     ["saga"] (ReqArg 
            (\arg opt -> opt { sagaFile = Just arg })
            "FILE") "saga file"
    , Option ['S']     ["spells"] (ReqArg 
            (\arg opt -> opt { spellDBFile = Just arg })
            "FILE") "input file for spell database"
    , Option ['t']     ["advance-to"] (ReqArg 
            (\arg opt -> opt { advanceSeason = Just arg })
            "SEASON") "advance to "
    , Option ['T']     ["sheet"] (ReqArg 
            (\arg opt -> opt { seasonFile = Just arg })
            "FILE") "output file for current character sheet"
    ]

armcharOpts :: [String] -> IO (Options, [String])
armcharOpts argv =
      case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: armchar [OPTION...] "

main :: IO ()
main = do 
     putStrLn "Starting: armchar ..."
     printTime
     args <- getArgs
     (opt,n) <- armcharOpts args
     putStrLns n
     putStrLn $ "Options: " ++ show opt

     main' opt

advChar :: Maybe String -> Maybe String -> Character -> IO ()
advChar Nothing _ _ = return ()
advChar sn fn cs0 = do
     writeMaybeOList fn $ printMD cs
     return ()
   where seasn = parseSeasonTime sn
         cs = advance seasn cs0

main' :: Options -> IO ()
main' opts | sagaFile opts /= Nothing = do 
     saga <- readSaga $ fromJust $ sagaFile opts
     case saga of
        Nothing -> error "Could not read Saga file"
        (Just s1) -> do
               writeSaga s1
               return ()
main' _ | otherwise = error "Not implemented!" 
