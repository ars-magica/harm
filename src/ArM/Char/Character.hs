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
                          , KeyPairList(..)
                          , KeyPair(..)
                          , FieldValue(..)
                          , characterStateName
                          , characterStartName
                          , parseSeasonTime
                          , Advancement(..)
                          , AugmentedAdvancement(..)
                          , AdvancementLike(..)
                          , prepareCharacter
                          , Advance(..)
                          , Season(..)
                          , SeasonTime(..)
                          , characterEntryTime
                          , isGameStart
                          ) where

import Data.Maybe 
import Data.List

import ArM.Char.Trait
import ArM.Char.Types.Advancement
import ArM.Char.Types.KeyPair
import ArM.Char.Types.Character
import ArM.Char.CharacterSheet
import ArM.Char.Validation
import ArM.Char.Virtues
import ArM.GameRules
import ArM.Helper

import ArM.Debug.Trace

-- | Character name with state identifier (current season)
characterStateName :: Character -> String
characterStateName c = fullName c ++ " - " ++ t
   where t | isNothing $ state c = "raw"
           | otherwise = show ( charTime $ fromJust $ state c )

-- | Character name with state identifier at Game Start
characterStartName :: Character -> String
characterStartName c = fullName c ++ " - " ++ "Game Start"

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x

-- | The current season of the character 
characterCurrentTime :: Character -> SeasonTime
characterCurrentTime = fromMaybe NoTime . fmap charTime . state

-- | The first season the character is played
isGameStart :: Character -> Bool
isGameStart = (==GameStart) . characterCurrentTime

-- |
-- = Advancements

-- | Apply advancement
-- This function is generic, and used for both chargen and ingame 
-- advancement.  The AugmentedAdvancement has to be prepared differently,
-- using either `prepareAdvancement` or `prepareCharGen`.
applyAdvancement :: AugmentedAdvancement
                 -> CharacterState 
                 -> (AugmentedAdvancement,CharacterState)
applyAdvancement a' cs = (a',cs')
    where cs' = cs { charTime = season a', traits = new }
          new = advanceTraitList change tmp
          tmp = sortTraits $ advanceTraitList inferred old 
          change = sortTraits $ changes a'
          inferred = inferredTraits a'
          old = sortTraits $ traits cs
          -- ag = fromMaybe 0 (augYears a') + age cs


-- | Infer traits from new virtues and flaws and add them to the advancement.
-- This typically applies to virtues providing supernatural abilities.
-- The ability is inferred and should not be added manually.
addInferredTraits :: Advancement -> AugmentedAdvancement
addInferredTraits a = defaultAA { inferredTraits = f a
                                , advancement = a
                                , augYears = yf }
     where f = inferTraits . getVF . changes 
           yf | Nothing /= advYears a = advYears a
              | isWinter $ season a = Just 1
              | otherwise = Nothing

-- | Inferred spell traits if Flawless Magic applies
flawlessSpells :: Bool -> AugmentedAdvancement -> AugmentedAdvancement
flawlessSpells False x = x
flawlessSpells True  x = x { inferredTraits = a ++ b }
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


-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF [] = []
getVF (p:ps) | isJust (virtue p) = g p:getVF ps
             | isJust (flaw p) = g p:getVF ps
             | otherwise = getVF ps
    where g = fromJust . computeTrait


-- | Sort the `inferredTraits` field of an `AugmentedAdvancement`
sortInferredTraits :: AugmentedAdvancement -> AugmentedAdvancement
sortInferredTraits x = x { inferredTraits = sortTraits $ inferredTraits x }

-- |
-- == Char Gen

-- | Compute the initial state if no state is recorded.
--
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
                  . initialLimits vfs       -- add XP limits etc to the advancement
                  . flawlessSpells (hasFlawless cs)
                  . addInferredTraits       -- infer traits from virtues and flaws
          where vfs = vfList sheet
                sheet = filterCS cs

-- | Infer an aging trait advancing the age according to the advancement
agingYears :: AugmentedAdvancement -> AugmentedAdvancement
agingYears x | y > 0 = x { inferredTraits = agePT y: inferredTraits x }
             | otherwise = x
   where y = fromMaybe 0 $ augYears x

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }

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
applyCharGenAdv a cs = applyAdvancement ( prepareCharGen cs a ) cs

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
-- == Validation

-- | validate an advancement, adding results to the validation field
validateCharGen :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen cs a 
           | m == "Virtues and Flaws" = validateVF cs a
           | m == "Characteristics" = validateChar cs a
           | otherwise = validateLevels $ validateXP a
           where m = fromMaybe "" $ mode a

-- | Validate allocation of virtues and flaws.
validateVF :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateVF sheet a 
             | m /= "Virtues and Flaws" = a
             | 0 /= f + v = a { validation = ValidationError imb:validation a }
             | v > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 (f,v) = calculateVFCost $ advancement a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and"
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ suf
                 val = "Virtues and flaws balance at " ++ show v ++ suf
                 suf = " of " ++ show lim ++ " points."
                 lim = vfLimit sheet

-- | Return the limit on flaw points, i.e. 3 for grogs and 10 for others.
vfLimit :: CharacterSheet -> Int
vfLimit sheet | Grog == csType sheet = 3
              | otherwise = 10

-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a


-- | Extract the virtue/flaw cost from a ProtoType; zero for other types of traits.
regCost :: ProtoTrait -> Int
regCost p | isJust (virtue p) = m p * f p
          | isJust (flaw p) = m p * f p
          | otherwise = 0
        where f = fromMaybe 0 . cost 
              m = fromMaybe 1 . multiplicity

-- | Validate points spent on characterics.
validateChar :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar sheet a | m /= "Characteristics" = a
             | ex < lim = a { validation = ValidationError und:validation a }
             | ex > lim = a { validation = ValidationError over:validation a }
             | otherwise = a { validation = Validated val:validation a }
           where m = fromMaybe "" $ mode a
                 lim = getCharAllowance $ vfList sheet
                 ex = calculateCharPoints $ advancement a
                 und = "Underspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 over = "Overspent " ++ (show ex) ++ " points out of "
                     ++ show lim ++ " on characteristics."  
                 val = "Correctly spent " ++ (show ex) ++ " points on characteristics."  

-- | Count characterics points spent in an Advancement
calculateCharPoints :: Advancement -> Int
calculateCharPoints = sum . map cScore . changes

-- | Count characterics points spent on a trait
cScore :: ProtoTrait -> Int
cScore p | isJust (characteristic p) = f p
         | otherwise = 0
        where f = pyramidScore . fromMaybe 0 . score 

-- | Calculate initial XP limits on Char Gen Advancements
initialLimits :: [ VF ] -> AugmentedAdvancement -> AugmentedAdvancement
initialLimits vfs ad
            | m == "Early Childhood" = ( f ad 45 ) { augYears = Just 5 }
            | m == "Apprenticeship" = app ad
            | m == "Characteristics" = f ad 0
            | m == "Later Life" = f ad $ laterLifeSQ vfs (advancement ad)
            | otherwise = ad { effectiveSQ = sourceQuality $ advancement ad  }
           where m = fromMaybe "" $ mode ad
                 f a x | isJust t = a { effectiveSQ = t }
                       | otherwise = a { effectiveSQ = Just x }
                 t = sourceQuality $ advancement ad
                 (app1,app2) = appSQ vfs
                 app a = a { effectiveSQ = Just app1, levelLimit = Just app2, augYears = Just 15 }

-- | Validate allocation of Spell Levels.
validateLevels :: AugmentedAdvancement -> AugmentedAdvancement
validateLevels a | isNothing (levelLimit a) = a
                 | sq > lsum = a { validation = und:validation a }
                 | sq < lsum = a { validation = over:validation a }
                 | otherwise = a { validation = val:validation a }
    where lsum = calculateLevels $ advancement a
          sq = fromMaybe 0 $ levelLimit a
          val = Validated $ "Correctly spent " ++ show sq ++ " spell levels."
          over = ValidationError $ "Overspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."

-- | Count spell levels from an Advancement
calculateLevels :: Advancement -> Int
calculateLevels = sum . map ( fromMaybe 0 . level ) . changes

-- |
-- == Advancement in Game


-- | Apply advancement
applyInGameAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyInGameAdv a cs = applyAdvancement ( prepareAdvancement cs a ) cs


-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement c = validate 
                     . sortInferredTraits   -- sort inferred traits
                     . inferSQ
                     . winterEvents c 
                     . flawlessSpells (hasFlawless c)
                     . addInferredTraits


-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> AugmentedAdvancement -- ^ Advancement 
             -> AugmentedAdvancement -- ^ modified Advancement
winterEvents c a | isWinter $ season a  
             = validateAging (y *> yl) agingOb  -- check for aging roll is made if required
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
                                    defaultPT { other = Just "Warping", points = Just 1 }
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
                   | isNothing (agingRoll ob) = trace ("No roll> "++show x) $ x { validation = err:validation x }
                   | otherwise =  x { validation = val:validation x }
              err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
              val = Validated $ "Aging roll made"

-- | Calculate initial XP limits on Advancements
inferSQ :: AugmentedAdvancement -> AugmentedAdvancement
inferSQ ad = ad { effectiveSQ = esq }
        where esq = maybeAdd (sourceQuality ad') (advBonus ad')
              ad' = advancement ad

class Advance a where
    -- | Advance the character until after the given time.
    advance :: SeasonTime -> a -> a
    -- | Advance the character one season forward
    step :: a -> a
    -- | Time of the next advancement of the character.
    nextSeason :: a -> SeasonTime

instance Advance Character where
   advance ct c | futureAdvancement c == [] = c
                      | isNothing (state c) = advance ct $ prepareCharacter c
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
                  (a,cs) = applyInGameAdv y (fromJust $ state c)

   nextSeason = f . futureAdvancement
       where f [] = NoTime
             f (x:_) = season x
