-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Inference
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Infer traits implied by others.
--
-- Only one function is exported: `addInference` which is used for both
-- in-game and chargen advancement. It must be used before all other processing
-- of the advancement, since it transforms an `Advancement` to an
-- `Augmented Advancement`.
--
-----------------------------------------------------------------------------
module ArM.Character.Inference (addInference) where

import ArM.Character.Winter
import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Character.CharacterSheet
import ArM.Story
import ArM.Trait
import ArM.Character.Virtues
import ArM.GameRules

import ArM.Debug.Trace

import Data.Maybe 
import Data.List 

-- | Infer traits a range of other traits, both from the new advancement
-- and the existing `Character`.
--
-- Currently included
-- 1. Inference of Decrepitude from Aging objects
-- 2. Inference from new Virtues and Flaws
-- 3. Inference from existing Virtues and Flaws
-- 4. Adding duration in years if required, including +1 in Winter.
-- 5. Winter Events, including
--     + Warping from the Longevity Ritual
--     + Validating Aging
addInference :: Character -> Advancement -> Augmented Advancement
addInference cs = winterEvents cs . inferAge . augmentAdvancement cs 

-- | Make the Augmented Advancement, by making inferences.
augmentAdvancement :: Character -> Advancement -> Augmented Advancement
augmentAdvancement cs a = Adv { explicitAdv = a
                        , inferredAdv = augmentAdvancement' cs a 
                        , validation = []
                        }

-- | Infer traits from other traits, returning an additional, inferred
-- Advancement object, adding also the duration in years, if necessary.
-- 
-- The trait inferences are made by the `inferProtoTraits` function.
augmentAdvancement' :: Character -> Advancement -> Advancement
augmentAdvancement' cs a = defaultAdvancement 
        { changes = inferProtoTraits cs xs
        , advSeason = season a
        , mode = mode a
        , years = yf }
     where xs = changes a
           yf | years a > 0 = years a
              | mode a == CharGen "Apprenticeship" = 15
              | mode a == CharGen "Early Childhood" = 5
              | isWinter $ season a = 1
              | otherwise = 0

-- | Infer additional ProtoTraits from a list of ProtoTrait objects.
-- This includes
-- 1. Inference of Decrepitude from Aging objects
-- 2. Inference from new Virtues and Flaws
-- 3. Inference from existing Virtues and Flaws
inferProtoTraits :: Character -> [ProtoTrait] -> [ProtoTrait]
inferProtoTraits cs xs =  g xs ++ f xs  ++ h xs
     where f =  inferTraits . getVF 
           g =  inferDecrepitude 
           h =   vfInference $ vfList cs 

inferAge :: Augmented Advancement -> Augmented Advancement
inferAge ad 
     | yr == 0 = ad
     | isJust ag = ad
     | otherwise = addChange a ad
     where ag = find ( (AgeKey ==) . traitKey ) $ changes cntad
           yr = years cntad 
           cntad = contractAdvancement ad
           a = trace ("[inferAge] " ++ show (agePT yr) ++ " [" ++ show (traitKey $ agePT yr) ++ "]") $ agePT yr

-- |
-- Infer Decrepitude points from aging points on characteristics
inferDecrepitude :: [ ProtoTrait ] -> [ ProtoTrait ]
inferDecrepitude [] = []
inferDecrepitude (x:xs) 
   | apts == 0 = inferDecrepitude xs
   | otherwise = d:inferDecrepitude xs
   where d = defaultPT { protoTrait = OtherTraitKey "Decrepitude",  points = Just apts }
         apts = fromMaybe 0 $ agingPts x


-- | Apply the effects of *Flawless  Magic` to the `ProtoTrait`s.
-- `ProtoTrait`s are added setting `flawless` to `True` for every
-- spell in the input list.
flawlessSpells :: [ProtoTrait] -> [ProtoTrait]
flawlessSpells [] = []
flawlessSpells (x:xs) | isSpell (protoTrait x) = y:ys
                       | otherwise = ys
    where ys = flawlessSpells xs
          y = defaultPT { protoTrait = protoTrait x, flawless = Just True }

-- | Infer prototraits from virtues and flaws.
-- Each virtue/flaw should have a `[ ProtoTrait ] -> [ ProtoTrait ]` function
-- which is applied when the virtue/flaw is found in the `[ VF ]` argument. 
vfInference :: [ VF ] -> [ ProtoTrait ] -> [ ProtoTrait ]
vfInference [] _ = []
vfInference (x:xs) ys
       | vfname x == "Elemental Magic" =  elementalMagic ys ++ vfInference xs ys
       | vfname x == "Flawless Magic" = flawlessSpells ys ++ vfInference xs ys
       | otherwise = vfInference xs ys

-- | Apply the effects of *Elemental Magic` to the `ProtoTrait`s.
elementalMagic :: [ ProtoTrait ] -> [ ProtoTrait ]
elementalMagic [] = [] 
elementalMagic (x:xs) | isEl "Te" x = mk "Ig" x:mk "Au" x:mk "Aq" x:elementalMagic xs
                      | isEl "Ig" x = mk "Te" x:mk "Au" x:mk "Aq" x:elementalMagic xs
                      | isEl "Au" x = mk "Te" x:mk "Ig" x:mk "Aq" x:elementalMagic xs
                      | isEl "Aq" x = mk "Te" x:mk "Ig" x:mk "Au" x:elementalMagic xs
                      | otherwise = elementalMagic xs
  where mk s y = defaultPT { protoTrait = ArtKey s
                   , bonusXP = Just $ xpround $ fromXP (fromMaybe 0 $ xp y) / 2.0
                   }
        isEl s y = protoTrait y == ArtKey s && isJust (xp y)


-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = x } }
