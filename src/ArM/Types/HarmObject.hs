module ArM.Types.HarmObject where

import ArM.Cov.Covenant
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

instance HarmObject Covenant where
    name = covenantName
    stateSeason = covenantSeason

