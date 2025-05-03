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
module ArM.Char.Validation (validate,validateCharGen,initialLimits) where

import ArM.Types.Advancement
import ArM.Types.ProtoTrait
import ArM.Types.Character
import ArM.Char.CharacterSheet
import ArM.Char.Virtues
import ArM.GameRules
import ArM.Helper

import Data.Maybe 

import ArM.Debug.Trace

-- |
-- = In-game Validation
-- In-game validation is relatively simple, depending only on the
-- `AugmentedAdvancement`.  Currently, only XP expenditure is validated.


-- |
-- Validate an in-game advancement, adding results to the validation field.
validate :: AugmentedAdvancement -> AugmentedAdvancement
validate a = a { inferredAdv = validateXP $ inferredAdv a }

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
validateXP :: Advancement -> Advancement
validateXP a | sq > xpsum = a { advCalidation = und:validation a }
             | sq < xpsum = a { advCalidation = over:validation a }
             | otherwise = a { validation = val:validation a }
    where xpsum = calculateXP $ advancement a
          sq = effectiveSQ a
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

-- | validate an advancement, adding results to the validation field
validateCharGen :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen sheet = validateCharGen' sheet 

validateCharGen' :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen' cs a 
           | m == CharGen "Virtues and Flaws" = validateVF cs a
           | m == CharGen "Characteristics" = validateChar cs a
           | otherwise = validateLevels $ validateXP a
           where m = mode a

-- | Validate allocation of virtues and flaws.
validateVF :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateVF sheet a 
             | m /= CharGen "Virtues and Flaws" = a
             | 0 /= f + v = a { validation = ValidationError imb:validation a }
             | v > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = mode a
                 (f,v) = calculateVFCost $ advancement a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and"
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ suf
                 val = "Virtues and flaws balance at " ++ show v ++ suf
                 suf = " of " ++ show lim ++ " points."
                 lim = vfLimit sheet

-- | Return the limit on flaw points, i.e. 3 for grogs and 10 for others.
vfLimit :: CharacterSheet -> Int
vfLimit sheet | Grog == csType sheet = 3
              | otherwise = 10

-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a


-- | Extract the virtue/flaw cost from a ProtoType; zero for other types of traits.
regCost :: ProtoTrait -> Int
regCost p | isJust (virtue p) = m p * f p
          | isJust (flaw p) = m p * f p
          | otherwise = 0
        where f = fromMaybe 0 . cost 
              m = fromMaybe 1 . multiplicity

-- | Calculate initial XP limits on Char Gen Advancements
initialLimits :: CharacterSheet -> Advancement -> Advancement
initialLimits sheet ad
            | m == CharGen "Early Childhood" = ( f ad 45 ) { advYears = Just 5 }
            | m == CharGen "Apprenticeship" = app ad
            | m == CharGen "Characteristics" = f ad 0
            | m == CharGen "Later Life" = f ad $ laterLifeSQ vfs (advancement ad)
            | otherwise = ad 
           where m = mode ad
                 f a x = a { sourceQuality = Just x }
                 (app1,app2) = appSQ vfs
                 app a = a { sourceQuality = Just app1, advSpellLevels = Just app2, advYears = Just 15 }
                 vfs = vfList sheet

-- | Validate allocation of Spell Levels.
validateLevels :: AugmentedAdvancement -> AugmentedAdvancement
validateLevels a | isNothing (levelLimit a) = a
                 | sq > lsum = a { validation = und:validation a }
                 | sq < lsum = a { validation = over:validation a }
                 | otherwise = a { validation = val:validation a }
    where lsum = calculateLevels $ advancement a
          sq = fromMaybe 0 $ levelLimit a
          val = Validated $ "Correctly spent " ++ show sq ++ " spell levels."
          over = ValidationError $ "Overspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."

-- | Count spell levels from an Advancement
calculateLevels :: Advancement -> Int
calculateLevels = sum . map ( fromMaybe 0 . level ) . changes

-- |
-- == Validation of Characteristics

-- | Validate points spent on characterics.
validateChar :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar sheet = f . validateChar' sheet
     where f x = x { advPostprocessTrait = PostProcessor processChar }

validateChar' :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar' sheet a | m /= CharGen "Characteristics" = a
             | ex < lim = a { validation = ValidationError und:validation a }
             | ex > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = mode a
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

