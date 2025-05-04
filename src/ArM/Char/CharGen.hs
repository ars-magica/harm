-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Character Generation
--
-- The module exports only one function, `prepareCharacter`, which
-- computes the character at game start, based on pre-game advancement
-- objects.
--
-- This function should be applied when a character is read from file
-- which does not include a persistent state.
--
-----------------------------------------------------------------------------
module ArM.Char.CharGen (prepareCharacter) where

import ArM.Char.Character
import ArM.Types.ProtoTrait
import ArM.Types
import ArM.GameRules

import Data.Maybe 

-- import ArM.Debug.Trace


-- |
-- = Char Gen

-- | Compute the initial state if no state is recorded.
-- The function uses `applyCGA` to process all of the pregame advancements.
-- It then calls `addConfidence` to add the confidence trait to the state
-- for the returned `Character` object
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



-- |
-- = CharGen Validation
-- 
-- CharGen validation is tricky, often depending on virtues and flaws.
-- Therefore, most functions depend also on the `CharacterSheet` in addition
-- to the `AugmentedAdvancement`.

-- | validate an advancement, adding results to the validation field
validateCharGen :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen sheet = validateLevels . validateXP . validateCharGen' sheet 

validateCharGen' :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateCharGen' cs a 
           | m == CharGen "Virtues and Flaws" = validateVF cs a
           | m == CharGen "Characteristics" = validateChar cs a
           | otherwise = a
           where m = mode a

validateVF :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateVF cs a = addValidation vfvs a
         where vfvs = (vfValidation cs) (explicitAdv a)

-- | Validate allocation of virtues and flaws.
vfValidation :: CharacterSheet -> Advancement -> [ Validation ]
vfValidation sheet a 
             | m /= CharGen "Virtues and Flaws" = []
             | 0 /= f + v = [ ValidationError imb ]
             | v > lim = [ ValidationError over ]
             | otherwise = [ Validated val ]
           where m = mode a
                 (f,v) = calculateVFCost a
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



-- | Validate allocation of Spell Levels.
validateLevels :: AugmentedAdvancement -> AugmentedAdvancement
validateLevels a | isNothing (spellLevels a) = a
                 | sq > lsum = addValidation [und] a 
                 | sq < lsum = addValidation [over] a
                 | otherwise = addValidation [val] a
    where lsum = spentLevels a
          sq = fromMaybe 0 $ spellLevels a
          val = Validated $ "Correctly spent " ++ show sq ++ " spell levels."
          over = ValidationError $ "Overspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."


-- |
-- == Validation of Characteristics

-- | Validate points spent on characterics.
validateChar :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar sheet = g . validateChar' sheet
     where f x = x { advPostprocessTrait = PostProcessor processChar }
           g x = x { inferredAdv = f $ inferredAdv x }

validateChar' :: CharacterSheet -> AugmentedAdvancement -> AugmentedAdvancement
validateChar' sheet a | m /= CharGen "Characteristics" = a
             | ex < lim = addValidation [ValidationError und] a
             | ex > lim = addValidation [ValidationError over] a
             | otherwise = addValidation [Validated val] a
           where m = mode a
                 lim = getCharAllowance $ vfList sheet
                 ex = calculateCharPoints $ explicitAdv a
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

