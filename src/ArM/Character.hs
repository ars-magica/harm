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
module ArM.Character (
                          -- * The Character Type
                          module ArM.Types.Character
                          -- * The CharacterSheet Type
                          , module ArM.Character.CharacterSheet
                          -- * The CharacterSheet Type
                          , module ArM.Types.Advancement
                          -- * Character Advancement
                          -- Inference
                          , addInference
                          -- Virtes
                          , inferTraits
                          , vfBonusSQ
                          , laterLifeSQ
                          , getCharAllowance
                          , inferConfidence
                          , appSQ
                          -- Character
                          , applyAdvancement 
                          -- * Combat statistics
                          , CombatLine(..)
                          , computeCombatStats
                          -- * Aging
                          , agePT
                          , charAgingBonusList
                          , charAgingBonus
                          -- * Convenience Functions
                          , characterEntryTime
                          , prepareCharacter
                          -- * Advancement
                          , prepareAdvancement
                          , validate
                          ) where

import ArM.Types.Advancement
import ArM.Types.Character
import ArM.Character.Inference
import ArM.Character.CharacterSheet
import ArM.Character.Virtues
import ArM.Character.Combat
import ArM.Character.CharGen
import ArM.Character.Character
import ArM.Character.Advancement
