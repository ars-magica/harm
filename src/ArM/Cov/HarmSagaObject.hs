{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.HarmSagaObject
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Stateful objects that can be looked up in a SagaState
--
-----------------------------------------------------------------------------
module ArM.Cov.HarmSagaObject where

-- import Data.Aeson
-- import Data.List
-- import GHC.Generics
-- import ArM.Debug.Trace
import ArM.Cov.Saga
import ArM.Types.HarmKey


class KeyObject h => HarmSagaObject h where
   -- | Get an object by key from a `SagaState` object
   harmGet :: SagaState -> HarmKey -> Maybe h
