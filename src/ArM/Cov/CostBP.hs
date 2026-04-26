{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.CostBP
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Cov.CostBP ( CostBP(..)) where

import Data.List 

import ArM.Types.Possession
import ArM.Types.Covenant
import ArM.Types.Library
import ArM.Char.Character
import ArM.Types

class CostBP t where
   -- | Calculate the BP (Build Point) cost of the possession.
   -- This is applies to covenant design [Cov].
   costBP :: t -> Int
instance CostBP LabText where
   costBP ob = error "Not implemented"
instance CostBP BookStats where
   costBP ob = error "Not implemented"
instance CostBP Enchantment where
   costBP ob = error "Not implemented"
instance CostBP Book where
   -- | The cost of a book is the sum of the costs of each book stats
   -- it provides.
   costBP = sum . costBP . bookStats
instance CostBP Possession where
   costBP = sum . map ($ob)
      where cs = [ sum . map costBP . bookTexts 
                 , sum . map costBP . labTexts 
		 , costBP . enchantment
		 ]
