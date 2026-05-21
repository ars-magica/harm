{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types for Character Traits and Possessions.
--
-- This module defines a range of mutually dependent types, which cannot (generally)
-- be split across modules because of these inter-dependencies.
--
-- The types cover not only character traits, but also possessions which apply to
-- both covenants and individual characters.  Books and enchanted devices are
-- examples of possessions.
--
-- This module proves a type for each kind of trait as well as a wrapper type,
-- `Trait` which can represent any kind of trait.
--
-- Possessions are autonomous objects, which may be transferred.  Poessions of
-- characters are wrapped as a trait.
--
-- Books are represented as `Book` objects for the sake of reading, but as possesions
-- they are wrapped as `Possession` object which may represent antologies.  The same
-- is the case for `LabText` objects.
--
-----------------------------------Types.------------------------------------------
module ArM.Trait ( 
         -- * The Trait Type
         Trait(..)
         , Ability(..)
         , Characteristic(..)
         , Art(..)
         , VF(..)
         , Spell(..)
         , PTrait(..)
         , OtherTrait(..)
         , CombatOption(..)
         , Confidence(..)
         , Reputation(..)

         -- * Convenience Functions
         , TraitClass(..)
         
         -- ** Sorting
         , (<:)
         , (>:)
         , sortTraits
         , findTrait

         -- * Aging
         , Age(..)
         , Aging
         , advanceAge
         , toAge
         , agingLimit
         , agingBonus
         , defaultAging
         , addYears
         , agingRoll

         -- * TraitKey
         , TraitKey(..)

         -- ** Convenience functions
         , fote
         , isSpell
         , isVF
         , artKey
         , artLongName
         , spellKeyName

         -- * Possessions
         , Possession(..)
         , defaultPossession
         , visArt
         , isNone

         -- ** Magic and Enchantments
         , Enchantment(..)
         , isMagic
         , isVis
         , isVisSrc
         , isAC
         , effectRDT

         -- ** Weapons and Mundane Equipment
         , isComposite
         , isMundaneEquipment
         , isWeapon
         , isArmour
         , isEquipment

         -- ** Books
         , BookStats(..)
         , Book(..)
         , defaultBook
         , BookDB(..)
         , isBook
         , isLabText
         , LabText(..)
         , wrapBooks
         , textLevel
         , readBookCSV

         -- * Weapon
         , Weapon(..)
         , Armour(..)
         -- * SpellRecord
         , SpellRecord(..)
         , MagicEffect(..)
         , SpellLike(..)
         , defaultSpellRecord
         , isRitual
         -- * Lab
         , Lab(..)
         , LabState(..)
         , LabVirtue(..)
         , LabBonus(..)
         , gq
         , getLabArt
         , health
         , usedSize
         , labVirtueLimit
         , totalBonus
         , labSafety
         , safety
         , baseSafety
         ) where

import ArM.Trait.Lab
import ArM.Trait.Trait
import ArM.Trait.SpellRecord
import ArM.Trait.Weapon
import ArM.Trait.Book
import ArM.Trait.Possession

import Data.List

-- ** Sorting Traits

-- | Compare two objects by their `traitKey`
(<:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(<:) p1 p2 = traitKey p1 < traitKey p2

-- | Compare two objects by their `traitKey`
(>:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(>:) p1 p2 = p2 <: p1


-- | Sort a list of traits by their 'traitKey'.
sortTraits :: TraitClass t => [ t ] -> [ t ]
sortTraits = sortBy f
       where f x y = compare (traitKey x) (traitKey y)

-- | Find a trait, given by a key, from a list of Trait objects.
findTrait :: (TraitClass a) => TraitKey -> [a] -> Maybe a
findTrait k = find ( (k==) . traitKey )
