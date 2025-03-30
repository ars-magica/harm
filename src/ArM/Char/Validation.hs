-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Validation
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to calculate and validate advancements
--
-- Only one function is exported: `validate`.
-- It checks the integrity of an AugmentedAdvancement object, and adds its
-- reports to the validation field.
--
-- This function should be used after all other calculation of the 
-- AugmentedAdvancement is complete, and before the object is displayed,
-- or written to file.
--
-----------------------------------------------------------------------------
module ArM.Char.Validation (validate,validateXP) where

import ArM.Char.Types.Advancement
import ArM.Char.Trait
import ArM.GameRules
import ArM.Helper

import Data.Maybe (fromMaybe,isJust)

import ArM.Debug.Trace

-- | Count regular XP (excluding reputation) from a ProtoTrait
regXP :: ProtoTrait -> XPType
regXP p | isJust (ability p) = f p
        | isJust (art p) = f p
        | isJust (spell p) = f p
        | otherwise = 0
        where f = fromMaybe 0 . xp 

-- | validate an advancement, adding results to the validation field
validate :: AugmentedAdvancement -> AugmentedAdvancement
validate a | otherwise = validateXP a

-- | Validate allocation of XP.
validateXP :: AugmentedAdvancement -> AugmentedAdvancement
validateXP a | sq > xpsum = a { validation = und:validation a }
             | sq < xpsum = trace ("Overspent> "++ show (advancement a)) $ a { validation = over:validation a }
             | otherwise = a { validation = val:validation a }
    where xpsum = calculateXP $ advancement a
          sq = fromMaybe 0 $ effectiveSQ a
          val = Validated $ "Correctly spent " ++ showNum sq ++ " xp."
          over = ValidationError $ "Overspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "."
          und = ValidationError $ "Underspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "."

-- | Count regular XP (excluding reputation) from an Advancement
calculateXP :: Advancement -> XPType
calculateXP = sum . map regXP . changes

