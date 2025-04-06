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
module ArM.Char.Validation (validate,validateCharGen,validateChar) where

import ArM.Char.Types.Advancement
import ArM.Char.Trait
import ArM.Char.CharacterSheet
import ArM.Char.Virtues
import ArM.GameRules
import ArM.Helper

import Data.Maybe (fromMaybe,isJust)

import ArM.Debug.Trace

-- |
-- = In-game Validation
-- In-game validation is relatively simple, depending only on the
-- `AugmentedAdvancement`.  Currently, only XP expenditure is validated.


-- |
-- Validate an in-game advancement, adding results to the validation field.
validate :: AugmentedAdvancement -> AugmentedAdvancement
validate a | otherwise = validateXP a

-- |
-- == XP Validation

-- | Count regular XP (excluding reputation) from a ProtoTrait
regXP :: ProtoTrait -> XPType
regXP p | isJust (ability p) = f p
        | isJust (art p) = f p
        | isJust (spell p) = f p
        | otherwise = 0
        where f = fromMaybe 0 . xp 


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

-- |
-- = CharGen Validation
-- 
-- CharGen validation is tricky, often depending on virtues and flaws.
-- Therefore, most functions depend also on the `CharacterSheet` in addition
-- to the `AugmentedAdvancement`.

-- |
-- == Validation of Characteristics

-- | Validate points spent on characterics.
validateChar :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar sheet = f . validateChar' sheet
     where f x = x { postProcessTrait = PostProcessor processChar }

validateChar' :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar' sheet a | m /= "Characteristics" = a
             | ex < lim = a { validation = ValidationError und:validation a }
             | ex > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 lim = getCharAllowance $ vfList sheet
                 ex = calculateCharPoints $ advancement a
                 und = "Underspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 over = "Overspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 val = "Correctly spent " ++ (show ex) ++ " points on characteristics."  



-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map cScore . changes

-- | Count characterics points spent on a trait
cScore :: ProtoTrait -> Int
cScore p | isJust (characteristic p) = f p
         | otherwise = 0
        where f = pyramidScore . fromMaybe 0 . score 

