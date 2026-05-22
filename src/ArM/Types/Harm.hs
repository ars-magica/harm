{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Harm
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Stateful objects that can be looked up in a SagaState
--
-- This module is kept separate from "ArM.Types.HarmObject" because it
-- declares instances for many types which import on HarmObject.
--
-----------------------------------------------------------------------------
module ArM.Types.Harm ( HarmSagaObject(..)
                      -- * Character
                      , Character(..)
                      , CharacterConcept(..)
                      , CharacterState(..)
                      , CharacterType(..)
                      , defaultCS
                      , fullConceptName
                      -- * Covenant
                      , Covenant(..)
                      , CovenantConcept(..)
                      , CovenantState(..)
                      , defaultCovState
                      , findCov
                      , covenant
                      -- * Saga
                      , Saga(..)
                      , SagaFile(..)
                      , SagaState(..)
                      , rootDir
                      , stateSeasons
                      , advSeasons
                      ) where

import ArM.Trait
import ArM.Types.Harm.Saga
import ArM.Story
import ArM.Types.Harm.Covenant
import ArM.Types.Harm.Character
import ArM.Helper


-- | It is possibly to search for a 'HarmObject' by 'HarmKey' throughout
-- a 'Saga' object.  The 'HarmSagaObject' class enables this.
--
-- **Caveat** This is not tested and not used at present.
class KeyObject h => HarmSagaObject h where
   -- | Get an object by key from a `SagaState` object
   harmGet :: Saga -> HarmKey -> Maybe h

instance HarmSagaObject Covenant where
   harmGet saga k = harmFind k $ covenants $ sagaState saga
instance HarmSagaObject Character where
   harmGet saga k = harmFind k $ characters $ sagaState saga
instance HarmSagaObject Lab where
   harmGet saga k = g $ map ( harmFind k . labs ) css
      where g [] = Nothing
            g (Nothing:xs) = g xs
            g (Just x:_) = Just x
            css = filterNothing $ map covenantState ( covenants $ sagaState saga )
