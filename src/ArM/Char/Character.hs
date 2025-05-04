{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains exports the types to process characters and 
-- advancement, including persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( module ArM.Types.Character
                          , module ArM.Types.Advancement
                          , module ArM.Char.Inference
                          , module ArM.Char.CharacterSheet
                          , module ArM.Char.Virtues
                          , characterEntryTime
                          , applyAdvancement 
                          , charAgingBonus
                          , agePT
                          ) where

import Data.Maybe 

import ArM.Types.Advancement
import ArM.Char.Inference
import ArM.Char.CharacterSheet
import ArM.Char.Virtues
import ArM.Types
import ArM.Types.ProtoTrait
-- import ArM.Helper
-- import ArM.Types.Library
import ArM.Types.Character
-- import ArM.Types.Trait

import ArM.Helper

-- |
-- = Convenience Functions for Character Properties

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x


charAgingBonus :: Character -> Int
charAgingBonus c = ag + lr + rb + cv + lh
    where ag = age c // 10
          lr = af longevityRitual -- Longevity Ritual 
          rb = af agingRollBonus -- Other personal bonus
          cv = 0 -- Covenant living condition
          lh = fromMaybe 0 $ fmap health (characterLab c) -- lab health bonus
          af f = fromMaybe 0 $ fmap f $ ageObject c       -- get stat from ageobject

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }

-- | Apply advancement
-- This function is generic, and used for both chargen and ingame 
-- advancement.  The AugmentedAdvancement has to be prepared differently,
-- using either `prepareAdvancement` or `prepareCharGen`.
applyAdvancement :: AugmentedAdvancement
                 -> CharacterState 
                 -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a,cs')
    where cs' = cs { charTime = season a, traits = new }
          new = advanceTraitList change tmp
          tmp = advanceTraitList inferred old
          change = sortTraits $ changes $ explicitAdv a
          inferred = sortTraits $ changes $ inferredAdv a
          old = sortTraits $ traits cs


