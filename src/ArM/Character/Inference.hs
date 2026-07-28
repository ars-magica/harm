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

import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Character.CharacterSheet
import ArM.Story
import ArM.Trait
import ArM.Character.Virtues
import ArM.GameRules

import Data.Maybe 

import ArM.Debug.Trace

-- | Infer traits a range of other traits, both from the new advancement
-- and the existing `Character`.
--
-- Currently included
-- 1. Increasing age by 1 in Winter
-- 2. Infer traits from virtues and flaws
-- 3. Infer decrepitude from aging points
-- 4. Infer the effects of Flawless magic
addInference :: Character -> Advancement -> Augmented Advancement
addInference cs a = Adv { explicitAdv = a
                        , inferredAdv = augmentAdvancement cs a 
                        , validation = []
                        }

-- | Infer traits from new virtues and flaws and add them to the advancement.
-- This typically applies to virtues providing supernatural abilities.
-- The ability is inferred and should not be added manually.
augmentAdvancement :: Character -> Advancement -> Advancement
augmentAdvancement cs a = defaultAdvancement 
        { changes = inferProtoTraits cs xs
        , advSeason = season a
        , mode = mode a
        , years = yf }
     where xs = changes a
           yf | isWinter $ season a = Just 1
              | otherwise = Nothing

inferProtoTraits :: Character -> [ProtoTrait] -> [ProtoTrait]
inferProtoTraits cs xs = trace "inferProtoTraits" $ g xs ++ f xs  ++ h xs
     where f =  inferTraits . getVF 
           g =  inferDecrepitude 
           h =  trace "vfInference" $ vfInference $ vfList cs 


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
       | vfname x == "Elemental Magic" = trace "Elemental" $ elementalMagic ys ++ vfInference xs ys
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
  where mk s x = defaultPT { protoTrait = ArtKey s
                   , bonusXP = Just $ xpround $ fromXP (fromMaybe 0 $ xp x) / 2.0
                   }
        isEl s x = protoTrait x == ArtKey s && isJust (xp x)


