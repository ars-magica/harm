{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains exports the types to process characters and 
-- advancement, including persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( module ArM.Types.Character
                          , module ArM.Char.Advancement
                          , module ArM.Char.CharacterSheet
                          , characterEntryTime
                          , prepareCharacter
                          , agingBonus
                          ) where

import Data.Maybe 

import ArM.Char.Advancement
import ArM.Char.CharacterSheet
import ArM.Types
-- import ArM.Helper
-- import ArM.Types.Library
import ArM.Types.Character
import ArM.Types.Trait

import ArM.Helper

-- |
-- = Convenience Functions for Character Properties

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x


agingBonus :: Character -> Int
agingBonus c = ag + lr + rb + cv + lh
    where ag = age c // 10
          lr = af longevityRitual -- Longevity Ritual 
          rb = af agingRollBonus -- Other personal bonus
          cv = 0 -- Covenant living condition
          lh = fromMaybe 0 $ fmap health (characterLab c) -- lab health bonus
          af f = fromMaybe 0 $ fmap f $ ageObject c       -- get stat from ageobject

-- |
-- = Char Gen

prepareCharacter :: Character -> Character
prepareCharacter c | state c /= Nothing = c
                   | otherwise = c { state = newstate
                                   , pregameDesign = xs
                                   , pregameAdvancement = []
                                   , entryTime = f $ futureAdvancement c
                                   }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA as defaultCS { charSType = charType $ concept c }
                  newstate = Just $ addConfidence $ cs { charTime = GameStart }
                  f [] = NoTime
                  f (x:_) = season x

-- | Augment and amend the advancements based on current virtues and flaws.
--
-- This function is applied by `applyCharGenAdv` before the advancement is
-- applied to the `CharacterState`.  It infers additional traits from 
-- virtues and flaws, add XP limits to the advancements, and checks that
-- the advancement does not overspend XP or exceed other limnits.
prepareCharGen :: CharacterState -> Advancement -> AugmentedAdvancement
prepareCharGen cs = validateCharGen sheet   -- Validate integrity of the advancement
                  . sortAdvTraits      -- Restore sort order on inferred traits
                  . agingYears              -- add years of aging as an inferred trait
                  . initialLimits (characterSheet cs)        -- infer additional properties on the advancement
                  . addInference cs         -- infer additional traits 
          where sheet = characterSheet cs

-- | Calculate initial XP limits on Char Gen Advancements
initialLimits :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
initialLimits sheet ad 
            | m == CharGen "Early Childhood" = sq 45 $ yr 5 ad
            | m == CharGen "Apprenticeship" = sq app1 $ lv app2 $ yr 15 ad
            | m == CharGen "Characteristics" = sq 0 ad
            | m == CharGen "Later Life" = sq (laterLifeSQ vfs ad) ad
            | otherwise = ad 
      where m = mode ad
            sq x a = a { inferredAdv = (inferredAdv a) { advSQ = Just x } }
            yr x a = a { inferredAdv = (inferredAdv a) { advYears = Just x } }
            lv x a = a { inferredAdv = (inferredAdv a) { advSpellLevels = Just x } }
            (app1,app2) = appSQ vfs
            vfs = vfList sheet

-- | Infer an aging trait advancing the age according to the advancement
agingYears :: AugmentedAdvancement -> AugmentedAdvancement
agingYears x | y > 0 = addProtoTrait [ agePT y ] x
             | otherwise = x
   where y = fromMaybe 0 $ years x


-- | Add the Confidence trait to the character state, using 
addConfidence :: CharacterState -> CharacterState
addConfidence cs = cs { traits = sortTraits $ ct:traits cs }
          where vfs = vfList sheet
                sheet = characterSheet cs
                ct | csType sheet == Grog = ConfidenceTrait $ Confidence
                           { cname = "Confidence", cscore = 0, cpoints = 0 }
                   | otherwise = inferConfidence vfs 


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv a cs = (a',f cs')
   where (a',cs') = applyAdvancement ( prepareCharGen cs a ) cs
         (PostProcessor g) = postprocessTrait a'
         f x = x { traits = map g $ traits x }

-- | Apply a list of advancements
applyCGA :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA a cs = applyCGA' ([],a,cs)

-- | Recursive helper for `applyCGA`.
applyCGA' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' (xs,[],cs) = (xs,cs)
applyCGA' (xs,y:ys,cs) = applyCGA' (a':xs,ys,cs')
    where (a',cs') = applyCharGenAdv y cs

