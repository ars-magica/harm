{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Internal.CostBP
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to calculate build point costs for covenant design.
--
--
-----------------------------------------------------------------------------
module ArM.Cov.Internal.CostBP ( CostBP(..) ) where

import Data.List 

import ArM.Types.Trait

class CostBP t where
   -- | Calculate the BP (Build Point) cost of the possession.
   -- This is applies to covenant design [Cov].
   costBP :: t -> Int

instance CostBP LabText where
   costBP =   (`div` 5) . (+1) . textLevel 

instance CostBP BookStats where
   costBP ob = error "Not implemented"

instance CostBP Enchantment where
   costBP MundaneItem = 0
   costBP (LesserItem eff) = 2*mag
      where mag = (effectLevel eff + 1) `div` 5
   costBP (GreaterDevice _ eff) = 2*(mag eff + 1) `div` 5
      where mag = sum . map effectLevel 
   costBP (Talisman _ eff) = 2*(mag eff + 1) `div` 5
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
                 ]
