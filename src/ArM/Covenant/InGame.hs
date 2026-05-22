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
import ArM.Covenant.Validation
import ArM.Covenant.Covenant
import ArM.Trait
import ArM.Story
import ArM.Helper
import Data.Maybe
import Data.List

-- | Initialise `Covenant` object for advancement
initAdvancement :: SeasonTime -> Covenant -> Covenant
initAdvancement t c = c { pastCovAdvancement = x:pastCovAdvancement c
                        , futureCovAdvancement = xs 
                        , covenantState = Just $ f (covenantState c) t
                        }
     where (x,xs) = iaHead t $ futureCovAdvancement c
           f Nothing t = defaultCovState { covTime = t }
           f (Just x) t = x { covTime = t }

-- | Make initial inferences on the advancement.
-- Currently no inference is made.
iaPrepare :: CovAdvancement -> Augmented CovAdvancement
iaPrepare a = Adv a noCovAdvancement []

-- | Empty augmented advancement object with the given time stamp
noAdvT :: SeasonTime -> Augmented CovAdvancement
noAdvT t = Adv a noCovAdvancement []
   where a = noCovAdvancement { caSeason = t }

-- | Empty augmented advancement object
noAdv :: Augmented CovAdvancement
noAdv = noAdvT NoTime

-- | Take the head off the future advancement if the time is right.
iaHead :: SeasonTime -> [CovAdvancement] -> (Augmented CovAdvancement,[CovAdvancement])
iaHead t [] = (noAdvT t,[])
iaHead t (x:xs) | season x == t = (iaPrepare x,xs)
                | otherwise = (noAdvT t,xs)

-- | Get the current contracted advancement being processed.
cvgCurrentAdv :: Covenant -> CovAdvancement
cvgCurrentAdv = contractAdvancement . fromMaybe noAdv . mhead . pastCovAdvancement

-- | Apply an advancement function to the CovenantState
stateApply :: ( CovenantState -> CovenantState ) -> Covenant -> Covenant
stateApply f c = c { covenantState = fmap f ( covenantState c ) }

-- | Advance covenfolk in the covenant
cvgCovenFolk :: Covenant -> Covenant
cvgCovenFolk c = stateApply (stepCovenFolk (cvgCurrentAdv c)) c

