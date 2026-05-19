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
-- This module is kept separate from "ArM.Types.HarmObject" because it
-- declares instances for many types which import on HarmObject.
--
-----------------------------------------------------------------------------
module ArM.Types.HarmSagaObject where

import ArM.Types.Saga
import ArM.Story
import ArM.Types.Covenant
import ArM.Types.Character
import ArM.Types.Lab
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
