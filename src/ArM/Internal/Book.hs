{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Internal.Book
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Internal.Book ( RawBook(..)
                         , readBookCSV
                         ) where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

import Data.Csv

data RawBook = RawBook 
             { key :: !String
             , traittype :: !String
             , trait :: !String
             , stats :: !String
             , title :: !String
             , creator :: !String
             , comment :: !String
             , copies :: !Int
             , language :: !String
             }


instance FromNamedRecord RawBook where
    parseNamedRecord r = RawBook 
                       <$> r .: "key" 
                       <*> r .: "traittype"
                       <*> r .: "trait"
                       <*> r .: "stats"
                       <*> r .: "title"
                       <*> r .: "creator"
                       <*> r .: "comment"
                       <*> r .: "copies"
                       <*> r .: "language"

readBookCSV :: String -> IO [RawBook]
readBookCSV fn = BL.readFile fn >>= (f . decodeByName)
   where f (Left err) = putStr err >> return []
         f (Right (_,v)) = return $ V.toList v

