-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharGen
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
-- This function should be applied when a character is read from file,
-- and it is not already advanced to `GameStart`.
--
-- The critical point which is specific for CharGen is `prepareCharGen`
-- which applies inference and validation to the advancement.
-- Once that is done, it is applied by `applyAdvancement` which is common
-- to all advancement.
--
-----------------------------------------------------------------------------
module ArM.Character.CharGen (prepareCharacter) where

import ArM.Character.Character
import ArM.Character.CharacterSheet
import ArM.Character.Inference
import ArM.Character.Validation
import ArM.Character.Virtues
import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Story
import ArM.Trait
import ArM.GameRules
import ArM.Helper

import Data.Maybe 

import ArM.Debug.Trace

-- * Char Gen

-- | Compute the initial state of the character.
--
-- Pregame advancements are processed recursively with `pregameAdvancement`.
-- Then, if the season of the character is `NoTime`, the season is update
-- to `GameStart`, `entryTime` is set if possible, and the confidence trait
-- is inferred and added.
prepareCharacter :: Character -> Character
prepareCharacter c
    | isNothing a = finaliseCharGen c
    | otherwise = trace ts $ prepareCharacter c''
    where as = pregameAdvancement c
          a = mhead as
          c'' = c' { pregameDesign = aa:pregameDesign c'
                   , pregameAdvancement = mtail as }
          (aa,c') = applyCharGenAdv (fromJust a) c
          ts = "CharGen: " ++ name c ++ " (" ++ advancementmode (fromJust a) ++ ")"

-- | Finalising CharGen implies setting the state time to `GameStart`, set the
-- entry time if possible, and add the confidence trait.
-- 
-- If the character state time is not `NoTime` this function is identity.
finaliseCharGen :: Character -> Character
finaliseCharGen c
   | charTime c /= NoTime = trace ("No pregame design for "++name c) c
   | otherwise = trace ("CharGen: "++name c) $ addConfidence $ setEntryTime c

-- | Set entry time and current time of the character
setEntryTime :: Character -> Character
setEntryTime c = c { entryTime = f $ futureAdvancement c, charTime = GameStart }
    where f [] = NoTime
          f (x:_) = season x

-- | Augment and amend the advancements based on current virtues and flaws.
--
-- This function is applied by `applyCharGenAdv` before the advancement is
-- applied to the `Character`.  It infers additional traits from 
-- virtues and flaws, add XP limits to the advancements, and checks that
-- the advancement does not overspend XP or exceed other limnits.
prepareCharGen :: Character -> Advancement -> Augmented Advancement
prepareCharGen cs 
   = validateCharGen cs      -- Validate integrity of the advancement
   . sortAdvTraits           -- Restore sort order on inferred traits
   . agingYears              -- add years of aging as an inferred trait
   . initialLimits cs        -- infer additional properties on the advancement
   . addInference cs         -- infer additional traits 

-- | Calculate initial XP limits on CharGen Advancements
initialLimits :: Character -> Augmented Advancement -> Augmented Advancement
initialLimits sheet ad 
            | m == CharGen "Early Childhood" = sq 120 $ yr 5 ad
            -- 120 xp includes native language
            | m == CharGen "Apprenticeship" = sq app1 $ lv app2 $ yr 15 ad
            | m == CharGen "Characteristics" = sq 0 ad
            | m == CharGen "Later Life" = sq (laterLifeSQ vfs ad) ad
            | otherwise = ad 
      where m = mode $ contractAdvancement ad
            sq x a = a { inferredAdv = (inferredAdv a) { sourceQuality = Just x } }
            yr x a = a { inferredAdv = (inferredAdv a) { years = Just x } }
            lv x a = a { inferredAdv = (inferredAdv a) { spellLevels = Just x } }
            (app1,app2) = appSQ vfs
            vfs = vfList sheet

-- | Infer an aging trait advancing the age according to the advancement
agingYears :: Augmented Advancement -> Augmented Advancement
agingYears x | y > 0 = addProtoTrait [ agePT y ] x
             | otherwise = x
   where y = fromMaybe 0 $ years $ contractAdvancement x


-- | Add the Confidence trait to the character state.
--
-- This is the last step of CharGen, inferring confidence from the traits.
addConfidence :: Character -> Character
addConfidence cs = cs { traits = sortTraits $ ct:traits cs }
          where vfs = vfList cs
                ct | isGrog cs = ConfidenceTrait $ Confidence
                           { cname = "Confidence", cscore = 0, cpoints = 0 }
                   | otherwise = inferConfidence vfs 


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> Character 
                -> (Augmented Advancement,Character)
applyCharGenAdv a cs = (a',f cs')
   where (a',cs') = applyAdvancement ( prepareCharGen cs a ) cs
         (PostProcessor g) = postprocessTrait $ contractAdvancement a'
         f x = x { traits = map g $ traits x }

-- * CharGen Validation
-- 
-- $chargenvalidation
-- CharGen validation is tricky, often depending on virtues and flaws.
-- Therefore, most functions depend also on the `Character` in addition
-- to the `Augmented Advancement`.

-- | validate an advancement, adding results to the validation field
validateCharGen :: Character -> Augmented Advancement -> Augmented Advancement
validateCharGen sheet = validateLevels . validateXP . validateCharGen' sheet 

validateCharGen' :: Character -> Augmented Advancement -> Augmented Advancement
validateCharGen' cs a 
           | m == CharGen "Virtues and Flaws" = validateVF cs a
           | m == CharGen "Characteristics" = validateChar cs a
           | otherwise = a
           where m = mode $ contractAdvancement a

validateVF :: Character -> Augmented Advancement -> Augmented Advancement
validateVF cs a = addValidation vfvs a
         where vfvs = (vfValidation cs) (explicitAdv a)

-- | Validate allocation of virtues and flaws.
vfValidation :: Character -> Advancement -> [ Validation ]
vfValidation sheet a 
             | m /= CharGen "Virtues and Flaws" = []
             | 0 /= f + v = [ ValidationError imb ]
             | v > lim = [ ValidationError over ]
             | otherwise = [ Validated val ]
           where m = mode a
                 (f,v) = calculateVFCost a
                 imb = "Virtues and flaws are imbalanced: "
                     ++ show v ++ " points of virtues and "
                     ++ show (-f) ++ " points of flaws."
                 over = "Exceeds limit on virtues; " ++ show v ++ suf
                 val = "Virtues and flaws balance at " ++ show v ++ suf
                 suf = " of " ++ show lim ++ " points."
                 lim = vfLimit sheet

-- | Return the limit on flaw points, i.e. 3 for grogs and 10 for others.
vfLimit :: Character -> Int
vfLimit sheet | isGrog sheet = 3
              | otherwise = 10

-- | Count virtue and flaw costs from an Advancement
calculateVFCost :: Advancement -> (Int,Int)
calculateVFCost a = ( sum $ filter (<0) rs, sum $ filter (>0) rs )
   where rs = map regCost $ changes a


-- | Extract the virtue/flaw cost from a ProtoType; zero for other types of traits.
regCost :: ProtoTrait -> Int
regCost p | isVF (protoTrait p) = m p * f p
          | otherwise = 0
        where f = fromMaybe 0 . cost 
              m = fromMaybe 1 . multiplicity



-- | Validate allocation of Spell Levels.
validateLevels :: Augmented Advancement -> Augmented Advancement
validateLevels a | isNothing (spellLevels $ contractAdvancement a) = a
                 | sq > lsum = addValidation [und] a 
                 | sq < lsum = addValidation [over] a
                 | otherwise = addValidation [val] a
    where lsum = spentLevels a
          sq = fromMaybe 0 $ spellLevels $ contractAdvancement a
          val = Validated $ "Correctly spent " ++ show sq ++ " spell levels."
          over = ValidationError $ "Overspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."
          und = ValidationError $ "Underspent " ++ show lsum ++ " spell levels of " ++ show sq ++ "."


-- |
-- == Validation of Characteristics

-- | Validate points spent on characterics.
validateChar :: Character -> Augmented Advancement -> Augmented Advancement
validateChar sheet = g . validateChar' sheet
     where f x = x { postprocessTrait = PostProcessor processChar }
           g x = x { inferredAdv = f $ inferredAdv x }

validateChar' :: Character -> Augmented Advancement -> Augmented Advancement
validateChar' sheet a | m /= CharGen "Characteristics" = a
             | ex < lim = addValidation [ValidationError und] a
             | ex > lim = addValidation [ValidationError over] a
             | otherwise = addValidation [Validated val] a
           where m = mode $ contractAdvancement a
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
cScore p = f (protoTrait p) p
        where f (CharacteristicKey _) = pyramidScore . fromMaybe 0 . score 
              f _ = \ _ -> 0

