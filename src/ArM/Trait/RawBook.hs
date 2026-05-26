{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait.RawBook
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Parse books from CSV.
--
-- The `readBookCSV` returns a list of `RawBook` objects which directly
-- maps from the CSV format.  
--
-----------------------------------------------------------------------------
module ArM.Trait.RawBook ( RawBook(..)
                         , readBookCSV
                         ) where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import ArM.Helper

import Data.Csv
import ArM.Debug.Trace

-- | Book type directly mapping the CSV format.
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
             } deriving ( Show )


-- | Vanilla parser based on the cassava library.
instance FromNamedRecord RawBook where
    parseNamedRecord r = RawBook 
                       <$> r .: "key" 
                       <*> r .: "traittype"
                       <*> r .: "trait"
                       <*> r .: "stats"
                       <*> r .: "title"
                       <*> r .: "creator"
                       <*> r .: "comment"
                       <*> fmap fix (r .: "copies")
                       <*> r .: "language"
             where fix = fix' . trim
                   fix' "" = 1
                   fix' x = read x
                   -- | The `fix` interprets a blank string as 1.

-- | Parse the given file and return a list of `RawBook` objects.
readBookCSV :: String        -- ^ CSV file
            -> IO [RawBook]  -- ^ List of books
readBookCSV fn = BL.readFile (ttrace fn) >>= (f . decodeByName)
   where f (Left err) = putStr err >> return []
         f (Right (_,v)) = return $ V.toList v

