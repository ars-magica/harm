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

import ArM.Char.Types.Advancement
import ArM.Char.Trait
import ArM.Char.Types.Character
import ArM.Char.CharacterSheet
import ArM.Types.Calendar
import ArM.Char.Virtues
-- import ArM.GameRules
-- import ArM.Helper

import Data.Maybe 

-- import ArM.Debug.Trace

-- | Infer traits a range of other traits, both from the new advancement
-- and the existing `CharacterState`.
addInference :: CharacterState -> Advancement -> AugmentedAdvancement
addInference cs = flawlessSpells cs . addInferredTraits

-- | Infer traits from new virtues and flaws and add them to the advancement.
-- This typically applies to virtues providing supernatural abilities.
-- The ability is inferred and should not be added manually.
addInferredTraits :: Advancement -> AugmentedAdvancement
addInferredTraits a = defaultAA { inferredTraits = g a ++ f a
                                , advancement = a
                                , augYears = yf }
     where f = inferTraits . getVF . changes 
           g = inferDecrepitude . changes
           yf | Nothing /= advYears a = advYears a
              | isWinter $ season a = Just 1
              | otherwise = Nothing

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
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
flawlessSpells :: CharacterState -> AugmentedAdvancement -> AugmentedAdvancement
flawlessSpells sheet x | hasFlawless sheet = x { inferredTraits = a ++ b }
                       | otherwise = x
     where a = flawlessSpells' $ changes $ advancement x
           b = inferredTraits x

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
    where ts = vfList $ filterCS c
          fms = filter ((=="Flawless Magic") . vfname ) ts

