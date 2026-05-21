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
import ArM.Helper

covAddValidation :: Augmented CovAdvancement -> Augmented CovAdvancement 
covAddValidation a =  a { validation = covGenValidation a ++ validation a }

covGenValidation :: Augmented CovAdvancement -> [ Validation ]
covGenValidation a = filterNothing [ bpValidation bp
                   , lossValidation ls, bhValidation bh ]
    where bp = acquired $ inferredAdv a
          ls = lost $ inferredAdv a
          bh = bhChanges $ inferredAdv a

lossValidation :: [ Possession ] -> Maybe Validation
lossValidation = f . sum . map costBP 
   where f 0 = Nothing
         f x = Just $ Validated $ "Lost possession for " ++ show x ++ " build points."

bpValidation :: [ Possession ] -> Maybe Validation
bpValidation = f . sum . map costBP 
   where f 0 = Nothing
         f x = Just $ Validated $ "Spent " ++ show x ++ " build points."

bhValidation :: [ VF ] -> Maybe Validation 
bhValidation vf 
    | h == b = f h
    | h < b = Just $ ValidationError ( "Overspent; "
                   ++ show b ++ "p boons against " 
                   ++ show h ++ "p hooks." ) 
    | otherwise = Just $ ValidationError ( "Underspent; " 
                       ++ show h ++ "p hooks against " 
                       ++ show b ++ "p boons." ) 
   where hooks = filter ( (<0) . vfcost ) vf
         boons = filter ( (>0) . vfcost ) vf
         h = sum $ map vfcost hooks
         b = sum $ map vfcost boons
         f 0 = Nothing
         f x = Just $ Validated $ "Hooks and boons balance at " ++ show x ++ " points."
