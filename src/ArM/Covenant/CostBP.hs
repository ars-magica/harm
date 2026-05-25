{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Covenant.CostBP
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to calculate build point costs for covenant design.
--
--
-----------------------------------------------------------------------------
module ArM.Covenant.CostBP ( CostBP(..) ) where

import ArM.Trait
import ArM.Helper
import ArM.Debug.Trace
import Data.Maybe
import Data.List
import Control.Monad

class CostBP t where
   -- | Calculate the BP (Build Point) cost of the possession.
   -- This is applies to covenant design [Cov].
   costBP :: t -> Int

instance CostBP LabText where
   costBP =   (// 5) . textLevel 

instance CostBP BookStats where
   costBP b = q (quality b) + ll (topic b)
      where l = fromMaybe 0 $ bookLevel b
            q Nothing = trace ("Error: No quality") 0
            q (Just x) = x 
            ll (AbilityKey _) = 3*l
            ll (ArtKey _) = l
            ll x = trace ("Error: book has topic " ++ show x) 0

instance CostBP Enchantment where
   costBP MundaneItem = 0
   costBP (LesserItem eff) = 2*mag
      where mag = (effectLevel eff + 1) // 5
   costBP (GreaterDevice _ eff) = 2*(mag eff + 1) // 5
      where mag = sum . map effectLevel 
   costBP (Talisman _ eff) = 2*(mag eff + 1) // 5
      where mag = sum . map effectLevel 
   costBP _ = error "Not implemented"

instance CostBP Book where
   -- | The cost of a book is the sum of the costs of each book stats
   -- it provides.
   costBP = sum . map costBP . bookStats

instance CostBP Possession where
   costBP ob = sum $ map ($ ob) cs
      where cs = [ sum . map costBP . bookTexts 
                 , sum . map costBP . labTexts 
                 , costBP . enchantment
                 , visBP
                 , (// 10) . silver
                 , specBP . staff
                 ]

            specBP (Just (Specialist xs)) = specialistBP xs
            specBP _ = 0

specialistBP :: [ Trait ] -> Int
specialistBP traits = teacherBP teach com mskill
    where abis = filterTrait traits
          com = fmap charScore . join . fmap getTrait
              $ findTrait (CharacteristicKey "Com") traits
          teach = fmap abilityScore $ find ((=="Teaching") . abilityName ) abis
          skills = filter ((/="Teaching") . abilityName ) abis
          mskill = foldr max 0  $ map abilityScore skills

teacherBP :: Maybe Int -> Maybe Int -> Int -> Int
teacherBP Nothing _ skill = skill
teacherBP (Just teach) Nothing skill = trace "Warning: Teacher without Com's"
                                     $ teach + skill
teacherBP (Just teach) (Just com) skill = teach + com + skill

-- | Build points for vis stock and vis sources
visBP :: Possession -> Int
visBP p = (5*visYield p) + (pawns p // 5)
