{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Covenant.Validation
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Covenants and functions for advancement.
--
-- Covenants use the `CovAdvancement` type from `ArM.Types.Advancement` with
-- some code shared with characters.  The code to advance a single step, as
-- well as the `covGen` function advancing to Game Start, are included here.
--
-- InGame advancement must be done jointly for all characters and covenants,
-- and this is handled by the `ArM.Advancement` module.
--
-----------------------------------------------------------------------------
module ArM.Covenant.Validation where

import ArM.Types.Advancement
import ArM.Covenant.CostBP
import ArM.Trait

covGenValidation :: Augmented CovAdvancement -> [ Validation ]
covGenValidation a = bpValidation bp ++ bhValidation bh
    where bp = acquired $ contractAdvancement a
          bh = bhChanges $ contractAdvancement a

bpValidation :: [ Possession ] -> [ Validation ]
bpValidation = bpVal . sum . map costBP 

bpVal :: Int -> [ Validation ]
bpVal 0 = []
bpVal x = [Validated $ "Spent " ++ show x ++ " build points."]

bhValidation :: [ VF ] -> [ Validation ]
bhValidation vf 
    | h == b = [ Validated $ "Hooks and boons balance at " ++ show h ++ " points." ]
    | h < b = [ ValidationError ( "Overspent; " ++ show b ++ "p boons against " 
                              ++ show h ++ "p hooks." ) ]
    | otherwise = [ ValidationError ( "Underspent; " ++ show h ++ "p hooks against " 
                              ++ show b ++  "p boons." ) ]
   where hooks = filter ( (<0) . vfcost ) vf
         boons = filter ( (>0) . vfcost ) vf
         h = sum $ map vfcost hooks
         b = sum $ map vfcost boons
