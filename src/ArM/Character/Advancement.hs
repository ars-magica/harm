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
-- Description :  Auxiliary functions for character advancement.
--
-- The only function exported is `winterEvents`.
--
-- In game, it is used by `ArM.Character.InGame.prepareAdvancement`.
-- 
-- It is not directly applicable to `CharGen` but the functionality
-- should be added also there.
--
-----------------------------------------------------------------------------
module ArM.Character.Advancement (winterEvents) where

import ArM.Character.Character
import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Trait
import ArM.Story

import ArM.Debug.Trace

import Data.Maybe
import Data.List

-- * Preparing the Advancement

-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: Character             -- ^ Current Character State
             -> Augmented Advancement -- ^ Advancement 
             -> Augmented Advancement -- ^ modified Advancement
winterEvents c a 
    | isWinter a = addVal $ a { inferredAdv = addAug $ inferredAdv a }
    | otherwise = a
    where ageOb = ageObject c
          y = age c
          ad = explicitAdv a
          -- check for aging roll is made if required
          pt = find ( (AgeKey ==) . traitKey ) $ changes ad
          -- Update stats
          addAug = addYear agingOb                -- add a yer of aging
                 . warpingLR                      -- add warping point for LR
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
          -- Validation
          addVal = validateAging (y >* yl) agingOb  
          validateAging False _ =  id
          validateAging True Nothing = addValidation  [err]
          validateAging True (Just ob) 
                   | isNothing (agingRoll ob) = addValidation [err]
                   | otherwise =  addValidation [val]
          err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
          val = Validated $ "Aging roll made"


-- | ProtoTrait representing the warping point from Longevity Ritual.
lrWarping :: ProtoTrait
lrWarping = defaultPT { protoTrait = OtherTraitKey "Warping"
                      , points = Just 1
                      , ptComment = Just "from Longevity Ritual" }


