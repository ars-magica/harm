-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Inference
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
-- `AugmentedAdvancement`.
--
-----------------------------------------------------------------------------
module ArM.Char.Inference (addInference) where

import ArM.Types.Advancement
import ArM.Types.ProtoTrait
import ArM.Types.Character
import ArM.Char.CharacterSheet
import ArM.Types
import ArM.Char.Virtues

import Data.Maybe 

-- import ArM.Debug.Trace

-- | Infer traits a range of other traits, both from the new advancement
-- and the existing `CharacterState`.
addInference :: CharacterState -> Advancement -> AugmentedAdvancement
addInference cs a = Adv { explicitAdv = a
                        , inferredAdv = augmentAdvancement cs a }

-- | Infer traits from new virtues and flaws and add them to the advancement.
-- This typically applies to virtues providing supernatural abilities.
-- The ability is inferred and should not be added manually.
augmentAdvancement :: CharacterState -> Advancement -> Advancement
augmentAdvancement cs a = defaultAdvancement 
        { advChanges = inferProtoTraits cs xs
        , advSeason = season a
        , advMode = mode a
        , advYears = yf }
     where xs = changes a
           yf | isWinter $ season a = Just 1
              | otherwise = Nothing

inferProtoTraits :: CharacterState -> [ProtoTrait] -> [ProtoTrait]
inferProtoTraits cs xs = g xs ++ f xs  ++ h xs
     where f =  inferTraits . getVF 
           g =  inferDecrepitude 
           h =  flawlessSpells cs 


-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isVF (protoTrait p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait

-- |
-- Infer Decrepitude points from aging points on characteristics
inferDecrepitude :: [ ProtoTrait ] -> [ ProtoTrait ]
inferDecrepitude [] = []
inferDecrepitude (x:xs) 
   | apts == 0 = inferDecrepitude xs
   | otherwise = d:inferDecrepitude xs
   where d = defaultPT { other = Just "Decrepitude",  points = Just apts }
         apts = fromMaybe 0 $ agingPts x


-- | Inferred spell traits if Flawless Magic applies
flawlessSpells :: CharacterState -> [ProtoTrait] -> [ProtoTrait]
flawlessSpells sheet xs | hasFlawless sheet = flawlessSpells' xs
                        | otherwise = []

-- | Inferred spell traits implementing Flawless Magic.
-- Auxiliary for `flawlessSpells`
flawlessSpells' :: [ProtoTrait] -> [ProtoTrait]
flawlessSpells' [] = []
flawlessSpells' (x:xs) | isNothing (spell x) = ys
                       | otherwise = y:ys
    where ys = flawlessSpells' xs
          y = defaultPT { spell = spell x, level = level x
                                      , tefo = tefo x
                                      , flawless = Just True
                                      }

-- | Does the character have Flawless Magic?
hasFlawless :: CharacterState -> Bool
hasFlawless c | fms == [] = False
              | otherwise = True
    where ts = vfList $ characterSheet c
          fms = filter ((=="Flawless Magic") . vfname ) ts

