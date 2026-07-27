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


-- | Inferred spell traits implementing Flawless Magic.
-- Auxiliary for `flawlessSpells`
flawlessSpells :: [ProtoTrait] -> [ProtoTrait]
flawlessSpells [] = []
flawlessSpells (x:xs) | isSpell (protoTrait x) = y:ys
                       | otherwise = ys
    where ys = flawlessSpells xs
          y = defaultPT { protoTrait = protoTrait x, flawless = Just True }

-- | Does the character have Flawless Magic?
hasFlawless :: Character -> Bool
hasFlawless c | fms == [] = False
              | otherwise = True
    where ts = vfList c
          fms = filter ((=="Flawless Magic") . vfname ) ts


vfInference :: [ VF ] -> [ ProtoTrait ] -> [ ProtoTrait ]
vfInference [] _ = []
vfInference (x:xs) ys
       | vfname x == "Elemental Magic" = trace "Elemental" $ elementalMagic ys ++ vfInference xs ys
       | vfname x == "Flawless Magic" = flawlessSpells ys ++ vfInference xs ys
       | otherwise = vfInference xs ys

elementalMagic :: [ ProtoTrait ] -> [ ProtoTrait ]
elementalMagic (x:xs) | isEl "Te" x = mk "Ig" x:mk "Au" x:mk "Aq" x:elementalMagic xs
                      | isEl "Ig" x = mk "Te" x:mk "Au" x:mk "Aq" x:elementalMagic xs
                      | isEl "Au" x = mk "Te" x:mk "Ig" x:mk "Aq" x:elementalMagic xs
                      | isEl "Aq" x = mk "Te" x:mk "Ig" x:mk "Au" x:elementalMagic xs
                      | otherwise = elementalMagic xs
elementalMagic [] = [] 

isEl :: String -> ProtoTrait -> Bool
isEl s x = protoTrait x == ArtKey s && isJust (xp x)
mk s x = defaultPT { protoTrait = ArtKey s
                   , bonusXP = Just $ trace "mk" $ ttrace $ xpround $ fromXP (fromMaybe 0 $ xp x) / 2.0
                   }

