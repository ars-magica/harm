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
module ArM.Char.Character ( module ArM.Char.Types.Character
                          , module ArM.Types.KeyPair
                          , module ArM.Types.Calendar
                          , module ArM.Char.Types.Advancement
                          , Advancement(..)
                          , prepareCharacter
                          , Advance(..)
                          , Season(..)
                          , SeasonTime(..)
                          , characterEntryTime
                          , HarmObject(..)
                          ) where

import Data.Maybe 
import Data.List

import ArM.Char.Trait
import ArM.Char.Types.Advancement
import ArM.Types.KeyPair
import ArM.Types.Calendar
import ArM.Char.Types.Character
import ArM.Char.CharacterSheet
import ArM.Char.Validation
import ArM.Char.Inference
import ArM.Char.Virtues
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = The Harm Object

-- |
-- The `HarmObject` class establishes a common interface for `Covenant` and
-- `Character`.
class HarmObject h where
    -- | Full name of the entity
    name :: h -> String

    -- | Current season of the object's stateY
    stateSeason :: h -> SeasonTime

    -- | String identifying the object and its state
    stateName :: h -> String
    stateName x = name x ++ " (" ++ show (stateSeason x) ++ ")"

    -- | The prepare function is applied when the object is read from file
    prepare :: h -> h
    prepare = id

    -- | Is the character state still at Game Start?
    isGameStart :: h -> Bool
    isGameStart = (==GameStart) . stateSeason

instance HarmObject Character where
    name = fullConceptName . concept
    stateSeason = characterSeason
    prepare = prepareCharacter


-- |
-- = Convenience Functions for Character Properties

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x

-- |
-- = Character Advancement

-- |
-- The Advance class represents objects which change state from
-- season to season.
class Advance a where
    -- | Advance the character until after the given time.
    advance :: SeasonTime -> a -> a
    -- | Advance the character one season forward
    step :: a -> a
    -- | Time of the next advancement of the character.
    nextSeason :: a -> SeasonTime

-- |
-- The implementation of character advancement depends on three auxiliary functions.
-- + `prepareCharacter` advances the character from a Nothing state to Game Start.
-- + `prepareAdvancement` augments the advancement object with limits and inference
-- + `applyAdvancement` applies an advancement to advance the character a single step.
--
-- Additional inference should be added to one of these functions.
-- + `prepareCharacter` (see below) if it applies to Character Generation only
-- + `prepareAdvancement` if it modifies the advancement only
-- + `applyAdvancement` if it modifies the CharacterState
instance Advance Character where
   advance ct c | isNothing (state c) = advance ct $ prepare c
                | futureAdvancement c == [] = c
                | ct < ct' = c
                | otherwise =  advance ct $ step c 
            where y =  head $ futureAdvancement c
                  ct' =  season y

   step c = c { state = Just cs 
              , pastAdvancement = (a:xs)
              , futureAdvancement = ys 
              }
            where (y:ys) = futureAdvancement c
                  xs = pastAdvancement c
                  (a,cs) = applyAdvancement (prepareAdvancement cstate y) cstate
                  cstate = fromJust $ state c

   nextSeason = f . futureAdvancement
       where f [] = NoTime
             f (x:_) = season x

-- | Apply advancement
-- This function is generic, and used for both chargen and ingame 
-- advancement.  The AugmentedAdvancement has to be prepared differently,
-- using either `prepareAdvancement` or `prepareCharGen`.
applyAdvancement :: AugmentedAdvancement
                 -> CharacterState 
                 -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a,cs')
    where cs' = cs { charTime = season a, traits = new }
          new = advanceTraitList change tmp
          tmp = advanceTraitList inferred old 
          change = sortTraits $ changes a
          inferred = sortTraits $ inferredTraits a
          old = sortTraits $ traits cs

applyAdv :: (Character,AugmentedAdvancement)
                 -> (Character,AugmentedAdvancement)
applyAdv (c,a) = (c',a')
    where (a',st') = applyAdvancement a st
          c' = c { state = Just st' }
          st = fromMaybe defaultCS $ state c


-- |
-- == Char Gen

-- | Compute the initial state if no state is recorded.
-- The function uses `applyCGA` to process all of the pregame advancements.
-- It then calls `addConfidence` to add the confidence trait to the state
-- for the returned `Character` object
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
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
                  . sortInferredTraits      -- Restore sort order on inferred traits
                  . agingYears              -- add years of aging as an inferred trait
                  . addInference cs         -- infer additional traits 
          where sheet = filterCS cs

-- | Infer an aging trait advancing the age according to the advancement
agingYears :: AugmentedAdvancement -> AugmentedAdvancement
agingYears x | y > 0 = x { inferredTraits = agePT y: inferredTraits x }
             | otherwise = x
   where y = fromMaybe 0 $ augYears x


-- | Add the Confidence trait to the character state, using 
addConfidence :: CharacterState -> CharacterState
addConfidence cs = cs { traits = sortTraits $ ct:traits cs }
          where vfs = vfList sheet
                sheet = filterCS cs
                ct | csType sheet == Grog = ConfidenceTrait $ Confidence
                           { cname = "Confidence", cscore = 0, cpoints = 0 }
                   | otherwise = inferConfidence vfs 


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv a cs = (a',f cs')
   where (a',cs') = applyAdvancement ( prepareCharGen cs a ) cs
         (PostProcessor g) = postProcessTrait a'
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


-- |
-- == Preparing the Advancement

-- |
-- Get the next augmented advancement.
nextAdvancement :: SeasonTime -> Character -> (Character,AugmentedAdvancement)
nextAdvancement ns ch | fs == [] = (ch,defaultAA)
                      | season adv >* ns = (ch,defaultAA)
                      | otherwise = (ch,a)
        where a = prepareAdvancement (fromJust st) adv
              st = state ch
              (adv:_) = fs
              fs = futureAdvancement ch

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement c = validate 
                     . sortInferredTraits   -- sort inferred traits
                     . inferSQ
                     . winterEvents c 
                     . addInference c

-- | Sort the `inferredTraits` field of an `AugmentedAdvancement`
sortInferredTraits :: AugmentedAdvancement -> AugmentedAdvancement
sortInferredTraits x = x { inferredTraits = sortTraits $ inferredTraits x }


-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> AugmentedAdvancement -- ^ Advancement 
             -> AugmentedAdvancement -- ^ modified Advancement
winterEvents c a | isWinter $ season a  
             = validateAging (y >* yl) agingOb  -- check for aging roll is made if required
             $ addYear agingOb                  -- add a yer of aging
             $ warpingLR a                      -- add warping point for LR
             | otherwise = a
        where ageOb = ageObject c
              y = age c
              pt = find ( (AgeKey ==) . traitKey ) $ changes a
              agingOb | isNothing pt = Nothing
                      | otherwise = aging $ fromJust pt
              lr | ageOb == Nothing = -1
                 | otherwise = longevityRitual $ fromJust ageOb
              yl | ageOb == Nothing = trace "No age object" 35
                 | otherwise = ageLimit $ fromJust ageOb
              warpingLR x | lr < 0 = x
                          | otherwise = x { inferredTraits = 
                                    defaultPT { other = Just "Warping"
                                              , points = Just 1
                                              , comment = Just "from Longevity Ritual" }
                                    :inferredTraits x }
              addYear o x | addsYear o = x
                          | otherwise = x { inferredTraits = agePT 1 :inferredTraits x }
              addsYear Nothing = False
              addsYear (Just x) | isNothing (addYears x) = False
                                | fromJust (addYears  x) <= 0 = False
                                | otherwise = True
              validateAging False _ x =  x
              validateAging True Nothing x = trace ("No aging> "++show a) $ x { validation = err:validation x }
              validateAging True (Just ob) x
                   | isNothing (agingRoll ob) = x { validation = err:validation x }
                   | otherwise =  x { validation = val:validation x }
              err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
              val = Validated $ "Aging roll made"

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }

-- | Calculate initial XP limits on Advancements
inferSQ :: AugmentedAdvancement -> AugmentedAdvancement
inferSQ ad = ad { effectiveSQ = esq }
        where esq = maybeAdd (sourceQuality ad') (advBonus ad')
              ad' = advancement ad


