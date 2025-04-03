module ArM.Types.HarmObject where

import ArM.Cov.Covenant
import ArM.Char.Character

class HarmObject h where
    name :: h -> String
    stateSeason :: h -> SeasonTime

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

