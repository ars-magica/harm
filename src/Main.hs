{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2022: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- This is the main module for the armchar web server.
-- This includes some wiring to reduce inter-dependencies between
-- other modules, and some configuration options which may later be
-- moved to data files or command line parameters.
--
-- * The data structure is loaded by the `getRawGraph` function
--   defined in `ArM.Load`
-- * Software Transactional Memory is set up by the `getState` function
--   from `ArM.STM`.
-- * The Web API is defined in the `ArM.WebServices` module.
--
-----------------------------------------------------------------------------

module Main where

-- Software Transactional Memory
import ArM.STM

-- Timer
import ArM.Time

-- Authentication

-- | The `authf` function validates the password in the Wai middleware
-- authf u p = return $ u == "user" && secureMemFromByteString p == password

-- | Saga File
sagaFile :: String
sagaFile = "Test/saga.ttl"

main :: IO ()
main = do 
     putStrLn "Starting: armchar-swish  ..."
     printTime
     stateVar <- loadSaga sagaFile
     putStrLn "Not implemented"
