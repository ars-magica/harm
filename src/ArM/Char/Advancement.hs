{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Advancement process and convenience functions for Characters.
--
-----------------------------------------------------------------------------
module ArM.Char.Advancement ( module ArM.Types.Advancement
                            , module ArM.Char.Virtues
                            , module ArM.Char.Inference
                            , module ArM.Char.Validation
                            , prepareAdvancement 
                            , applyAdvancement 
                            , agePT
                            ) where

import ArM.Types.Advancement
import ArM.Types.Character
import ArM.Types.ProtoTrait
-- import ArM.Types.Trait
import ArM.Types.Library
import ArM.Types
import ArM.Char.Virtues
import ArM.Char.Inference
import ArM.Char.Validation
import ArM.Char.CharacterSheet
import ArM.GameRules

import ArM.Debug.Trace

import Data.Maybe
import Data.List

-- |
-- = Preparing the Advancement


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement c = validate 
                     . sortInferredTraits   -- sort inferred traits
                     . inferSQ c
                     . winterEvents c 
                     . addInference c


-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> AugmentedAdvancement -- ^ Advancement 
             -> AugmentedAdvancement -- ^ modified Advancement
winterEvents c a | isWinter $ season a  
             = validateAging (y >* yl) agingOb  -- check for aging roll is made if required
             $ addYear agingOb                  -- add a yer of aging
             $ warpingLR a                      -- add warping point for LR
             | otherwise = a
        where ageOb = ageObject c
              y = age c
              pt = find ( (AgeKey ==) . traitKey ) $ changes a
              agingOb | isNothing pt = Nothing
                      | otherwise = aging $ fromJust pt
              lr | ageOb == Nothing = -1
                 | otherwise = longevityRitual $ fromJust ageOb
              yl | ageOb == Nothing = trace "No age object" 35
                 | otherwise = ageLimit $ fromJust ageOb
              warpingLR x | lr < 0 = x
                          | otherwise = x { inferredTraits = 
                                    defaultPT { other = Just "Warping"
                                              , points = Just 1
                                              , comment = Just "from Longevity Ritual" }
                                    :inferredTraits x }
              addYear o x | addsYear o = x
                          | otherwise = x { inferredTraits = agePT 1 :inferredTraits x }
              addsYear Nothing = False
              addsYear (Just x) | isNothing (addYears x) = False
                                | fromJust (addYears  x) <= 0 = False
                                | otherwise = True
              validateAging False _ x =  x
              validateAging True Nothing x = trace ("No aging> "++show a) $ x { validation = err:validation x }
              validateAging True (Just ob) x
                   | isNothing (agingRoll ob) = x { validation = err:validation x }
                   | otherwise =  x { validation = val:validation x }
              err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
              val = Validated $ "Aging roll made"


-- | Calculate initial XP limits on Advancements
inferSQ :: CharacterState -> AugmentedAdvancement -> AugmentedAdvancement
inferSQ cs ad = ad { baseSQ = sq, bonusSQ = vfBonusSQ vf ad }
        where vf = vfList $ filterCS cs
              (sq,cap) = getSQ ad
-- Infer SQ for Exposure = 2
-- Infer SQ for reading from book
-- Infer SQ for taught from teacher
-- Infer SQ for adventure from covenant

bookSQ :: AugmentedAdvancement -> AugmentedAdvancement 
bookSQ aa | isNothing stats = aa
          | isNothing tr = aa
          | otherwise = aa 
    where tr = ttrace $ primaryXPTrait $ advancement aa
          stats = find ctp $ foldl (++) [] $ map bookStats $ bookUsed aa
          ctp =  (==(fromJust tr)) . topic 


getSQ :: AugmentedAdvancement -> (Maybe XPType,Maybe Int)
getSQ a | isExposure ad = (Just 2,Nothing)
        | mode ad == Reading = rd 
        | otherwise = mstat
   where ad = advancement a
         mstat = (sourceQuality ad,sourceCap ad)
         rd = (fmap fromIntegral $ quality bk,bookLevel bk)
         bk = head $ bookStats $ head $ bookUsed a

-- |
-- Calculate the Source Quality the character generates as a teacher.
charTeacherSQ :: CharacterState -> Int
charTeacherSQ cs = 3 + com + tch
    where sheet = filterCS cs
          com = sheetCharacteristicScore sheet (CharacteristicKey "Com")
          (tch,tspec) = sheetAbilityScore sheet (CharacteristicKey "Teaching")
          -- add good teacher
          -- subtract flaws
          -- add speciality
          -- add one/two student bonus
-- Teacher SQ +

-- |
-- = Applying the Advancement

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
          change = sortTraits $ changes a
          inferred = sortTraits $ inferredTraits a
          old = sortTraits $ traits cs


-- |
-- == Convenience Functions (Exported)

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }
