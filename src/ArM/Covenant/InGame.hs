{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Covenant.InGame
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  In-Game advancement of Covenants.
--
--
-----------------------------------------------------------------------------
module ArM.Covenant.InGame where

import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Covenant.Covenant
import ArM.Story
import ArM.Helper
import Data.Maybe

-- | Initialise `Covenant` object for advancement
initCovAdvancement :: SeasonTime -> Covenant -> Covenant
initCovAdvancement t c = c { pastCovAdvancement = x:pastCovAdvancement c
                        , futureCovAdvancement = xs 
                        , covTime = t
                        }
     where (x,xs) = icaHead t $ futureCovAdvancement c

-- | Make initial inferences on the advancement.
-- Currently no inference is made.
icaPrepare :: CovAdvancement -> Augmented CovAdvancement
icaPrepare a = Adv a noCovAdvancement []

-- | Empty augmented advancement object with the given time stamp
noAdvT :: SeasonTime -> Augmented CovAdvancement
noAdvT t = Adv a noCovAdvancement []
   where a = noCovAdvancement { caSeason = t }

-- | Empty augmented advancement object
noAdv :: Augmented CovAdvancement
noAdv = noAdvT NoTime

-- | Take the head off the future advancement if the time is right.
icaHead :: SeasonTime -> [CovAdvancement] -> (Augmented CovAdvancement,[CovAdvancement])
icaHead t [] = (noAdvT t,[])
icaHead t (x:xs) | season x == t = (icaPrepare x,xs)
                | otherwise = (noAdvT t,xs)

-- | Get the current contracted advancement being processed.
cvgCurrentAdv :: Covenant -> CovAdvancement
cvgCurrentAdv = contractAdvancement . fromMaybe noAdv . mhead . pastCovAdvancement

-- | Advance covenfolk in the covenant
cvgCovenFolk :: Covenant -> Covenant
cvgCovenFolk c = stepCovenFolk (cvgCurrentAdv c) c

-- | Apply one CovAdvancement object to the `CovenantState`.
-- This is the same for pre-game and in-game advancement.
cvgStep :: Covenant -> Covenant
cvgStep c = stepBH adv $ stepPossessions adv c
          where adv = cvgCurrentAdv c
