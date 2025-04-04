-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Types.HarmObject
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Generic interface for covenants and characters
--
-- These definitions may have to be moved, since they easily creates 
-- circular dependencies.
--
-----------------------------------------------------------------------------
module ArM.Types.HarmObject where

import ArM.Char.Character

-- | The `HarmObject` class establishes a common interface for `Covenant` and
-- `Character`.
class HarmObject h where
    -- | Full name of the entity
    name :: h -> String

    -- | Current season of the object's stateY
    stateSeason :: h -> SeasonTime

    -- | String identifying the object and its state
    stateName :: h -> String
    stateName x = name x ++ " (" ++ show (stateSeason x) ++ ")"

    -- | The prepare function is applied when the object is read from file
    prepare :: h -> h
    prepare = id

instance HarmObject Character where
    name = fullConceptName . concept
    stateSeason = characterSeason
    prepare = prepareCharacter


