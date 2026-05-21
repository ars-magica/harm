{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Saga.DB
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Database types
--
-----------------------------------Types.------------------------------------------

module ArM.Saga.DB ( BookDB(..)
                    , BookDB'(..)
                    ) where

import ArM.Trait.Trait
import ArM.Types.Harm
import ArM.Story
import Control.Monad

-- | The `BookDB` class is any type wherein one may look up books by
-- their ID.
class BookDB' h where
   -- | Look up a book by key (String) in a database.
   booklookup :: h -> String -> Maybe Book
   booklookup db k = lookupbook k db 
   -- | Look up a book by key (String) in a database.
   -- This is equivalent to `bookLookup` with the arguments swapped
   lookupbook :: String -> h -> Maybe Book
   lookupbook k db = booklookup db k

instance (BookDB' h) => BookDB' [h] where
   lookupbook k = foldl mplus Nothing . map (\ x -> booklookup x k) 
instance BookDB' Book where
   booklookup bk k | k == bookID bk = Just bk
                   | otherwise = Nothing

-- | The `BookDB` class is any type wherein one may look up books by
-- their ID.
class BookDB h where
   -- | Look up a book by key (String) in a database.
   bookLookup :: h -> HarmKey -> Maybe Possession
   bookLookup db k = lookupBook k db 
   -- | Look up a book by key (String) in a database.
   -- This is equivalent to `bookLookup` with the arguments swapped
   lookupBook :: HarmKey -> h -> Maybe Possession
   lookupBook k db = bookLookup db k

instance (BookDB h) => BookDB [h] where
   lookupBook k = foldl mplus Nothing . map (\ x -> bookLookup x k) 
instance BookDB Possession where
   bookLookup bk k | k == harmKey bk = Just bk
                   | length bs > 0 = Just bk
                   | otherwise = Nothing
       where bs = filter ( (==k) . harmKey ) $ bookTexts bk


instance BookDB Covenant where
   lookupBook k = join . fmap (lookupBook k) . covenantState 

instance BookDB CovenantState where
    lookupBook k = lookupBook k . possessions

