{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Covenant
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
module ArM.Cov.Covenant ( module ArM.Types.Covenant
           , CostBP
           -- * Covenant Generation and Advancement
           , covGen
           , stepCovState
           ) where

import ArM.Types.Covenant
import ArM.Types.Trait
import ArM.Types.Advancement
import ArM.Cov.Internal.CostBP
import ArM.Helper
import ArM.Debug.Trace
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
stepCovState st adv = stepPossessions adv $ stepBooks adv $ stepCovenFolk adv st

-- | Apply one pre-game CovAdvancement to the `Covenant`.
-- This is an auxiliary for `covGen`.
genStep :: Covenant -> CovAdvancement -> Covenant
genStep cov adv = trace "genStep" $ cov { covenantState = Just st'
                            , covenantPregame = aa:covenantPregame cov }
   where st' = stepCovState st adv
         st = fromMaybe defaultCovState $ covenantState cov
         aa = Adv adv noCovAdvancement

-- | Advance the `covenfolk` attribute of the `CovenantState`.
stepCovenFolk :: CovAdvancement -> CovenantState -> CovenantState
stepCovenFolk aa st = st { covenFolkID = cid }
   where cid1 = sort $ joining aa ++ covenFolkID st 
         cid = cid1 -= ( sort $ leaving aa )
-- | Advance the `library` attribute of the `CovenantState`.
stepBooks :: CovAdvancement -> CovenantState -> CovenantState
stepBooks aa st = st { library = bid }
   where bid1 = sort $ acquired aa ++ library st 
         bid = bid1 -= ( sort $ lost aa )
-- | Advance the `possessions` attribute of the `CovenantState`.
stepPossessions :: CovAdvancement -> CovenantState -> CovenantState
stepPossessions aa st = st { possessions = bid }
   where bid1 = sort $ acquired' aa ++ possessions st 
         bid = bid1 -= ( sort $ lost' aa )
