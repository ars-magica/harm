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
-- Description :  Auxiliary functions for character ddvancement.
--
-- In game, these functions are used by `ArM.Character.InGame.prepareAdvancement`.
-- 
-- Some of the functions may also apply to chargen - such as `winterEvents`.
--
-----------------------------------------------------------------------------
module ArM.Character.Advancement where

import ArM.Character.Character
import ArM.Character.CharacterSheet
import ArM.Character.Virtues
import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Trait
import ArM.Story
import ArM.GameRules

import ArM.Debug.Trace

import Data.Maybe
import Data.List

-- * Preparing the Advancement

-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
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


