{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.HarmSagaObject
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Stateful objects that can be looked up in a SagaState
--
-----------------------------------------------------------------------------
module ArM.Types.HarmSagaObject where

-- import Data.Aeson
-- import Data.List
-- import GHC.Generics
-- import ArM.Debug.Trace
import ArM.Types.Saga
import ArM.Types.HarmObject
import ArM.Types.Covenant
import ArM.Types.Character


class KeyObject h => HarmSagaObject h where
   -- | Get an object by key from a `SagaState` object
   harmGet :: SagaState -> HarmKey -> Maybe h

instance HarmSagaObject Covenant where
   harmGet st k = harmFind k $ covenants st
instance HarmSagaObject Character where
   harmGet st k = harmFind k $ characters st
