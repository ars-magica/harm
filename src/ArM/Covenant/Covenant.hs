{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Covenant.Covenant
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
module ArM.Covenant.Covenant where

import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Covenant.Validation
import ArM.Trait
import ArM.Story
import ArM.Helper
import Data.Maybe
import Data.List

-- ** Covenant Generation and Advancement

-- | Apply the covenant design and advance the covenant to Game Start.
covGen :: Covenant -> Covenant
covGen cov = foldl genStep cov' as
   where as = covenantDesign cov
         cov' = cov { covenantDesign = [] }

-- | Apply one CovAdvancement object to the `CovenantState`.
-- This is the same for pre-game and in-game advancement.
stepCovState :: CovenantState -> CovAdvancement -> CovenantState
stepCovState st adv = stepBH adv $ stepPossessions adv st

-- | Apply one pre-game CovAdvancement to the `Covenant`.
-- This is an auxiliary for `covGen`.
genStep :: Covenant -> CovAdvancement -> Covenant
genStep cov adv = cov { covenantState = Just st'
                      , covenantPregame = aa:covenantPregame cov }
   where st' = stepCovState st adv
         st = fromMaybe defaultCovState $ covenantState cov
         aa = covAddValidation $ Adv adv noCovAdvancement []

-- | Advance the `covenfolk` attribute of the `CovenantState`.
stepCovenFolk :: CovAdvancement -> CovenantState -> CovenantState
stepCovenFolk aa st = st { covenFolkID = cid }
   where cid1 = sort $ joining aa ++ covenFolkID st 
         cid = cid1 -= ( sort $ leaving aa )

-- | Update boons and hooks
stepBH :: CovAdvancement -> CovenantState -> CovenantState
stepBH aa st = st { boonhook = mergeBH bh1 bh2 }
   where bh1 = sort $ boonhook st 
         bh2 = sort $ bhChanges aa

-- | merge the new and the old list of boons and hooks.
-- Auxiliary for `stepBH`.
mergeBH :: [VF] -> [VF] -> [VF]
mergeBH [] xs = xs
mergeBH xs [] = xs
mergeBH (x:xs) (y:ys) 
    | traitKey x < traitKey y = x:mergeBH xs (y:ys)
    | traitKey x > traitKey y = y:mergeBH (x:xs) ys
    | n == 0 = mergeBH (x:xs) ys
    | otherwise  = y { vfMultiplicity = n}:mergeBH xs ys
    where n = count x + count y

-- | Advance the `possessions` attribute of the `CovenantState`.
stepPossessions :: CovAdvancement -> CovenantState -> CovenantState
stepPossessions aa st = st { possessions = bid }
   where bid1 = sort $ acquired aa ++ possessions st 
         bid = bid1 -= ( sort $ lost aa )
