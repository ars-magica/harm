{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Functions
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Character Traits, including Abilities, Spells, Virtues, etc..
--
-- This module proves a type for each kind of trait as well as a wrapper type,
-- `Trait` which can represent any kind of trait.
--
-- This module defines the types as well as the `TraitType` class, and instances
-- for `show`, sorting, and JSON.
--
-----------------------------------Types.------------------------------------------
module ArM.Types.Functions where 

-- import ArM.Types.Trait
import ArM.Types

-- ** Sorting Traits

(<:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(<:) p1 p2 = traitKey p1 < traitKey p2

{-
(>:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(>:) p1 p2 = p2 <: p1
-}


sortTraits :: TraitClass t => [ t ] -> [ t ]
sortTraits = sortBy f
       where f x y = compare (traitKey x) (traitKey y)

-- | Find a trait, given by a key, from a list of Trait objects.
findTrait :: (TraitClass a) => TraitKey -> [a] -> Maybe a
findTrait k = find ( (k==) . traitKey )

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

-- | Get the unique identifier of an original book
bookKey :: Book -> HarmKey
bookKey = BookKey . bookID


-- * CSV

readStats :: String -> (Maybe Int, Maybe Int)
readStats "" = trace "empty book stats" (Nothing, Nothing)
readStats "Spell" = trace "empty book stats" (Nothing, Nothing)
readStats (' ':xs) = readStats xs
readStats ('Q':xs) = (Nothing, Just $ readMaybeInt xs)
readStats ('L':xs) = (lvl ys, ql ys)
        where ys = map ( readMaybeInt . unpack ) $ splitOn "Q" $ pack xs
              lvl = maybeHead 
              ql (_:x:_) = Just x
              ql _ = Nothing
readStats x = trace ( "no parse: " ++ x ) (Nothing, Nothing)

readMaybeInt :: String -> Int
readMaybeInt = fromMaybe (-1) . readMaybe

readTopic :: String -> String -> TraitKey
readTopic x = readTopic' (trim x)
readTopic' :: String -> String -> TraitKey
readTopic' "Art" y  = ArtKey $ trim y
readTopic' "Ability" y  = AbilityKey $ trim y
-- readTopic' "Spell" y  = SpellKey $ trim y
readTopic' _ y  = AbilityKey $ trim y


makeBookStats :: String   -- ^ trait type
              -> String   -- ^ trait label
              -> String   -- ^ Stat String
              -> BookStats -- ^ Book stat object
makeBookStats x y z = BookStats 
         { topic = readTopic x y
         , quality = q
         , bookLevel = l
         , reread = 1
         } where (l,q) = readStats z

-- * Convenience Functions

isMundaneEquipment :: Possession -> Bool
isMundaneEquipment p = isEquipment p && (not . isMagic) p

isEquipment :: Possession -> Bool
isEquipment p = not $ foldl (||) False [ f p | f <- fs ] 
   where fs = [ isVis, isWeapon, isArmour, isAC, isBook ]

-- ** Books

-- | Is the book a tractatus or something else?
isTractatus :: Book -> Bool
isTractatus = f . bookStats 
    where f [] = False
          f (x:_) = isJust ( quality x ) && isNothing ( bookLevel x )

-- | Is the item a book?
isBook :: Possession -> Bool
isBook p = bookTexts p /= []

-- | Does the item contain lab texts?
isLabText :: Possession -> Bool
isLabText p = labTexts p /= []

-- | Wrap a book as a possesion
wrapBook :: Book -> Possession
wrapBook b = defaultPossession 
             { bookTexts = [ b ]
             , itemCount = bookCount b
             , itemName = bookTitle b
             }

-- | Wrap a list of books as possesions
wrapBooks :: [Book] -> [Possession]
wrapBooks = map wrapBook

-- ** Magic Effects

effectRDT :: MagicEffect -> String
effectRDT eff = showStrList [ r, d, t ]
   where r = f "Range" (effectRange eff)
         d = f "Duration" (effectDuration eff)
         t = f "Target" (effectTarget eff)
         f _ "" = ""
         f s x = s ++ ": " ++ x


