{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Winter
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Auxiliary functions for character advancement.
--
-- The only function exported is `winterEvents`.
--
-----------------------------------------------------------------------------
module ArM.Character.Winter (winterEvents) where

import ArM.Character.Character
import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Trait
import ArM.Story

import ArM.Debug.Trace

import Data.Maybe
import Data.List

-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: Character             -- ^ Current Character State
             -> Augmented Advancement -- ^ Advancement 
             -> Augmented Advancement -- ^ modified Advancement
winterEvents c = addWarping c . ageValidation c

ageValidation :: Character             -- ^ Current Character State
             -> Augmented Advancement -- ^ Advancement 
             -> Augmented Advancement -- ^ modified Advancement
ageValidation c a 
    | years ad > 0 = addVal a
    | otherwise = a
    where ageOb = ageObject c
          y = age c
          ad = contractAdvancement a
          -- check for aging roll is made if required
          pt = find ( (AgeKey ==) . traitKey ) $ changes ad
          -- Update stats
          agingOb | isNothing pt = Nothing
                      | otherwise = aging $ fromJust pt
          yl | ageOb == Nothing = trace "No age object" 35
             | otherwise = ageLimit $ fromJust ageOb

          -- Validation
          addVal = validateAging (y >* yl) agingOb  
          validateAging False _ =  id
          validateAging True Nothing = addValidation  [err]
          validateAging True (Just ob) 
                   | isNothing (agingRoll ob) = addValidation [err]
                   | otherwise =  addValidation [val]
          err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
          val = Validated $ "Aging roll made"

getLR :: Character -> Int
getLR = fromMaybe 0 . fmap longevityRitual . ageObject

addWarping :: Character             -- ^ Current Character State
             -> Augmented Advancement -- ^ Advancement 
             -> Augmented Advancement -- ^ modified Advancement
addWarping c | getLR c <= 0 = id 
             | otherwise = addChange lrWarping


-- | ProtoTrait representing the warping point from Longevity Ritual.
lrWarping :: ProtoTrait
lrWarping = defaultPT { protoTrait = OtherTraitKey "Warping"
                      , points = Just 1
                      , ptComment = [ "from Longevity Ritual" ] }


