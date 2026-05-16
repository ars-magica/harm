{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  In-game character ddvancement.
--
-- Only a single function, `prepareAdvancement`, is exported.
-- It applice to in-game character advancement only.
-- Its purpose is to make those inferences which can be made on the
-- individual character in isolation.
--
-----------------------------------------------------------------------------
module ArM.Character.Advancement ( prepareAdvancement, validate ) where

import ArM.Character.Character
import ArM.Character.Inference
import ArM.Character.CharacterSheet
import ArM.Character.Virtues
import ArM.Types.Character
import ArM.Types.Advancement
import ArM.Types.ProtoTrait
import ArM.Trait
import ArM.Types
import ArM.GameRules

import ArM.Debug.Trace

import Data.Maybe
import Data.List

-- * Preparing the Advancement

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> Augmented Advancement
prepareAdvancement c = sortAdvTraits   -- sort inferred traits
                     . winterEvents c 
                     . addInference c


-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> Augmented Advancement -- ^ Advancement 
             -> Augmented Advancement -- ^ modified Advancement
winterEvents c a | isWinter a = Adv { explicitAdv = ad, inferredAdv = aa' }

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
          lr | ageOb == Nothing = 0
                 | otherwise = longevityRitual $ fromJust ageOb
          yl | ageOb == Nothing = trace "No age object" 35
                 | otherwise = ageLimit $ fromJust ageOb
          warpingLR x | lr <= 0 = x
                      | otherwise = x { changes = lrWarping:changes x }
          addYear o x | addsYear o = x
                      | otherwise = x { changes = agePT 1:changes x }
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
inferSQ :: Character -> Augmented Advancement -> Augmented Advancement
inferSQ cs ad = ad { inferredAdv = aa { sourceQuality = sq, bonusSQ = vfBonusSQ vf ad } }
        where vf = vfList $ characterSheet cs
              (sq,_) = getSQ ad
              aa = inferredAdv ad
-- Infer SQ for Exposure = 2
-- Infer SQ for reading from book
-- Infer SQ for taught from teacher
-- Infer SQ for adventure from covenant

{-
bookSQ :: Augmented Advancement -> Augmented Advancement 
bookSQ aa | isNothing stats = aa
          | isNothing tr = aa
          | otherwise = aa 
    where tr = ttrace $ primaryXPTrait $ advancement aa
          stats = find ctp $ foldl (++) [] $ map bookStats $ bookUsed aa
          ctp =  (==(fromJust tr)) . topic 
-}

getSQ :: Augmented Advancement -> (Maybe XPType,Maybe Int)
getSQ a | isExposure ad = (Just 2,Nothing)
        -- | mode ad == Reading = rd bks
        | otherwise = mstat
   where ad = explicitAdv a
         mstat = (sourceQuality ad,sourceCap ad)
         -- rd [] = (Nothing,Nothing)
         -- rd (bk:bs) = (fmap fromIntegral $ quality bk,bookLevel bk)
         -- bks | usd == [] = []
             -- | otherwise = bookStats $ head usd
         -- usd = bookUsed a

-- |
-- == Convenience Functions

-- | ProtoTrait representing the warping point from Longevity Ritual.
lrWarping :: ProtoTrait
lrWarping = defaultPT { protoTrait = OtherTraitKey "Warping"
                      , points = Just 1
                      , ptComment = Just "from Longevity Ritual" }


-- * In-game Validation
--
-- In-game validation is relatively simple, depending only on the
-- `Augmented Advancement`.  Currently, only XP expenditure is validated.

-- | Add source qualities and validate XP expenditure
validate :: Character -> Augmented Advancement -> Augmented Advancement
validate c = validateXP . inferSQ c
