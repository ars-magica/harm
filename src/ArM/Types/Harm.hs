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
                      , CharacterType(..)
                      , fullConceptName
                      -- * Covenant
                      , Covenant(..)
                      , CovenantConcept(..)
                      , CovenantState(..)
                      , defaultCovState
                      , findCov
                      -- * Saga
                      , Saga(..)
                      , SagaFile(..)
                      , SagaState(..)
                      , rootDir
                      , stateSeasons
                      , advSeasons
                      , newSagaState
                      ) where

import ArM.Trait
import ArM.Types.Harm.Saga
import ArM.Story
import ArM.Types.Harm.Covenant
import ArM.Types.Harm.Character
import ArM.Helper

import qualified Data.Map as M

-- | It is possibly to search for a 'HarmObject' by 'HarmKey' throughout
-- a 'Saga' object.  The 'HarmSagaObject' class enables this.
--
-- **Caveat** This is not tested and not used at present.
class KeyObject h => HarmSagaObject h where
   -- | Get an object by key from a `SagaState` object
   harmGet :: Saga -> HarmKey -> Maybe h
   harmGet s k = harmLookup k $ sagaState s
   -- | Look up an object in a SagaState
   harmLookup :: HarmKey -> SagaState -> Maybe h
   -- | Turn a list of objects into a Map
   toDB :: [h] -> M.Map String h
   toDB = M.fromList . toPairs
   -- | Map objects to (key,object) pairs.
   toPairs :: [h] -> [(String,h)]
   toPairs = filterNothing . map toPair 
   -- | Map an object to (key,object) pair.
   -- This extracts the String ID from the `HarmKey` object, and keys of
   -- the wrong type give Nothing.
   toPair :: h -> Maybe (String,h)

instance HarmSagaObject Covenant where
   harmLookup (CovenantKey k) = M.lookup k . covenants
   harmLookup _ = \ _ -> Nothing
   toPair = f . g
      where f (CovenantKey k,ob) = Just (k,ob)
            f _ = Nothing
            g ob = (harmKey ob,ob)
instance HarmSagaObject Character where
   harmLookup (CharacterKey k) = M.lookup k . characters
   harmLookup _ = \ _ -> Nothing
   toPair = f . g
      where f (CharacterKey k,ob) = Just (k,ob)
            f _ = Nothing
            g ob = (harmKey ob,ob)
instance HarmSagaObject Lab where
   harmLookup k saga = g $ M.mapMaybe ( harmFind k . labs ) css
      where g = mhead . M.elems
            css = M.mapMaybe covenantState ( covenants saga )
   toPair = f . g
      where f (LabKey k,ob) = Just (k,ob)
            f _ = Nothing
            g ob = (harmKey ob,ob)

-- | Create a new CharacterState, giving covenants and characters as list
newSagaState :: String -> [Covenant] -> [Character] -> SagaState
newSagaState t cvs chs = SagaState
         { stateTitle = t
         , seasonTime = GameStart
         , covenants = toDB cvs
         , characters = toDB chs
         }
