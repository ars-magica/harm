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
                          , module ArM.Char.Advancement
                          , module ArM.Char.CharacterSheet
                          , characterEntryTime
                          , agingBonus
                          ) where

import Data.Maybe 

import ArM.Char.Advancement
import ArM.Char.CharacterSheet
import ArM.Types
import ArM.Helper
import ArM.Types.Character
import ArM.Types.Trait

-- |
-- = Convenience Functions for Character Properties

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x


agingBonus :: Character -> Int
agingBonus c = ag + lr + rb + cv + lh
    where ag = age c // 10
          lr = af longevityRitual -- Longevity Ritual 
          rb = af agingRollBonus -- Other personal bonus
          cv = 0 -- Covenant living condition
          lh = fromMaybe 0 $ fmap health (characterLab c) -- lab health bonus
          af f = fromMaybe 0 $ fmap f $ ageObject c       -- get stat from ageobject

