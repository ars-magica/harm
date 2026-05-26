{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait.Book
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Functions to manage reading of Book objects from file
--
-----------------------------------Types.------------------------------------------
module ArM.Trait.Book ( readBookCSV ) where

import ArM.Helper
import ArM.Trait.Trait
import qualified ArM.Trait.RawBook as IB

import ArM.Debug.Trace

import Data.Maybe
import Data.Text       (splitOn, unpack, pack)
import Text.Read             (readMaybe)


-- | Translate from the `RawBook` format used by the CSV parser to 
-- the `Book` format.
fromRawBook :: IB.RawBook -> Book
fromRawBook rb = 
      defaultBook { bookID = IB.key  rb
                , bookTitle = IB.title rb
                , bookStats = [ makeBookStats (IB.traittype rb) (IB.trait rb) (IB.stats rb) ]
                , bookAuthor = IB.creator rb
                , bookAnnotation = [ IB.comment rb ]
                , bookCount = IB.copies rb
                , bookLanguage = Just $ IB.language rb
                }

-- | Parse the given file and return a list of books.
readBookCSV :: String -> IO [Book]
readBookCSV fn = IB.readBookCSV fn >>= return . map fromRawBook

-- * Library

{-
-- | Get the unique identifier of an original book
bookKey :: Book -> HarmKey
bookKey = BookKey . bookID
-}


-- * CSV

readStats :: String -> (Maybe Int, Maybe Int)
readStats "" = trace "empty book stats" (Nothing, Nothing)
readStats "Spell" = trace "empty book stats" (Nothing, Nothing)
readStats (' ':xs) = readStats xs
readStats ('Q':xs) = (Nothing, Just $ readMaybeInt xs)
readStats ('L':xs) = (mhead ys, ql ys)
        where ys = map ( readMaybeInt . unpack ) $ splitOn "Q" $ pack xs
              ql (_:x:_) = Just x
              ql _ = Nothing
readStats x = trace ( "no parse: " ++ x ) (Nothing, Nothing)

readMaybeInt :: String -> Int
readMaybeInt = fromMaybe (-1) . readMaybe

readTopic :: String -> String -> TraitKey
readTopic x y = readTopic' (trim x) (trim y)
readTopic' :: String -> String -> TraitKey
readTopic' "Art" y  = ArtKey y
readTopic' "Ability" y  = AbilityKey y
-- readTopic' "Spell" y  = SpellKey y
readTopic' _ y  = AbilityKey $ trim y


makeBookStats :: String   -- ^ trait type
              -> String   -- ^ trait label
              -> String   -- ^ Stat String
              -> BookStats -- ^ Book stat object
makeBookStats x y z = trace "makeBookStats"
         $ trace x
         $ trace y
         $ trace z
         $ ttrace
         $ BookStats 
         { topic = ttrace $ readTopic x y
         , quality = ttrace $ q
         , bookLevel = ttrace $ l
         , reread = 1
         } where (l,q) = readStats z

