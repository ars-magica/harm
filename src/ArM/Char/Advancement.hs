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

-- Module reexported
import ArM.Types.Advancement
import ArM.Char.Virtues
import ArM.Char.Inference
import ArM.Char.Validation

-- Other Types
import ArM.Types.Character
import ArM.Types.ProtoTrait
import ArM.Types.Library
import ArM.Types

-- Other modules
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
                     . sortAdvTraits   -- sort inferred traits
                     . inferSQ c
                     . winterEvents c 
                     . addInference c

lrWarping :: ProtoTrait
lrWarping = defaultPT { other = Just "Warping"
                      , points = Just 1
                      , ptComment = Just "from Longevity Ritual" }

-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> AugmentedAdvancement -- ^ Advancement 
             -> AugmentedAdvancement -- ^ modified Advancement
winterEvents c a | isWinter $ season a = Adv { explicitAdv = ad, inferredAdv = aa' }

             | otherwise = a
    where ageOb = ageObject c
          y = age c
          ad = explicitAdv a
          aa = inferredAdv a
          -- check for aging roll is made if required
          aa' = validateAging (y >* yl) agingOb  
                  $ addYear agingOb                  -- add a yer of aging
                  $ warpingLR aa                     -- add warping point for LR
          pt = find ( (AgeKey ==) . traitKey ) $ changes ad
          agingOb | isNothing pt = Nothing
                      | otherwise = aging $ fromJust pt
          lr | ageOb == Nothing = -1
                 | otherwise = longevityRitual $ fromJust ageOb
          yl | ageOb == Nothing = trace "No age object" 35
                 | otherwise = ageLimit $ fromJust ageOb
          warpingLR x | lr < 0 = x
                      | otherwise = x { advChanges = lrWarping:advChanges x }
          addYear o x | addsYear o = x
                      | otherwise = x { advChanges = agePT 1:advChanges x }
          addsYear Nothing = False
          addsYear (Just x) | isNothing (addYears x) = False
                            | fromJust (addYears  x) <= 0 = False
                            | otherwise = True
          validateAging False _ =  id
          validateAging True Nothing = addValidation  [err]
          validateAging True (Just ob) 
                   | isNothing (agingRoll ob) = addValidation [err]
                   | otherwise =  addValidation [val]
          err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
          val = Validated $ "Aging roll made"


-- | Calculate initial XP limits on Advancements
inferSQ :: CharacterState -> AugmentedAdvancement -> AugmentedAdvancement
inferSQ cs ad = ad { inferredAdv = aa { advSQ = sq, advBonus = vfBonusSQ vf ad } }
        where vf = vfList $ characterSheet cs
              (sq,cap) = getSQ ad
              aa = inferredAdv ad
-- Infer SQ for Exposure = 2
-- Infer SQ for reading from book
-- Infer SQ for taught from teacher
-- Infer SQ for adventure from covenant

{-
bookSQ :: AugmentedAdvancement -> AugmentedAdvancement 
bookSQ aa | isNothing stats = aa
          | isNothing tr = aa
          | otherwise = aa 
    where tr = ttrace $ primaryXPTrait $ advancement aa
          stats = find ctp $ foldl (++) [] $ map bookStats $ bookUsed aa
          ctp =  (==(fromJust tr)) . topic 
-}

getSQ :: AugmentedAdvancement -> (Maybe XPType,Maybe Int)
getSQ a | isExposure ad = (Just 2,Nothing)
        -- | mode ad == Reading = rd bks
        | otherwise = mstat
   where ad = explicitAdv a
         mstat = (sourceQuality ad,sourceCap ad)
         rd [] = (Nothing,Nothing)
         rd (bk:bs) = (fmap fromIntegral $ quality bk,bookLevel bk)
         -- bks | usd == [] = []
             -- | otherwise = bookStats $ head usd
         -- usd = bookUsed a

-- |
-- Calculate the Source Quality the character generates as a teacher.
charTeacherSQ :: CharacterState -> Int
charTeacherSQ cs = 3 + com + tch
    where sheet = characterSheet cs
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
          change = sortTraits $ changes $ explicitAdv a
          inferred = ttrace $ sortTraits $ changes $ inferredAdv a
          old = sortTraits $ traits cs


-- |
-- == Convenience Functions (Exported)

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }
