{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Character
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
-- Character advancement is divided conceptually into CharGen (pre-game advancement)
-- and in-game advancement.  Functions that are specific for one or the other requires
-- the import of
-- + `ArM.Character.CharGen` for character generation
-- + `ArM.Character.Advancement` for in-game ddvancement
--
--
-----------------------------------------------------------------------------
module ArM.Character.Character (
                          applyAdvancement 
                          -- * Aging
                          , charAgingBonusList
                          , charAgingBonus
                          -- * Convenience Functions
                          , module ArM.Character.CharacterSheet
                          ) where

import Data.Maybe 

import ArM.Types.Advancement
import ArM.Character.CharacterSheet
import ArM.Story
import ArM.Trait
import ArM.Helper
import ArM.Types.Harm

charAgingBonus :: Character -> Int
charAgingBonus c = ag - sum ( map snd (charAgingBonusList c) )
    where ag = age c // 10

charAgingBonusList :: Character -> [ (String,Int) ]
charAgingBonusList c = [ ( "Longevity Ritual", af longevityRitual )
                       , ( "Personal Bonus", af agingRollBonus )
                       , ( "Living Conditions", cv )
                       , ( "Lab Health Bonus", lh )
                       ]
    where cv = 0 -- Covenant living condition
          lh = (`div`2) $ fromMaybe 0 $ fmap health (characterLab c) -- lab health bonus
          af f = fromMaybe 0 $ fmap f $ ageObject c       -- get stat from ageobject


-- | Apply advancement
-- This function is generic, and used for both chargen and ingame 
-- advancement.  The Augmented Advancement has to be prepared differently,
-- using either `prepareAdvancement` or `prepareCharGen`.
applyAdvancement :: Augmented Advancement
                 -> Character
                 -> (Augmented Advancement,Character)
applyAdvancement a cs = (a,cs')
    where cs' = cs { charTime = season a, traits = new }
          new = advanceTraitList change tmp
          tmp = advanceTraitList inferred old
          change = sortTraits $ changes $ explicitAdv a
          inferred = sortTraits $ changes $ inferredAdv a
          old = sortTraits $ traits cs

