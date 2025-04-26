{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Types.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
		    , sagaState
		    , sagaStateName
                    ) where


import Data.Maybe 
import Data.Aeson 
import Data.List 
import GHC.Generics

import ArM.Char.Character
import ArM.DB.Spell
import ArM.DB.Weapon
import ArM.Types.Covenant
import ArM.Types.Library
import ArM.Types.HarmKey
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Saga type

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaTitle :: String
         , sagaStates :: [ SagaState ]
         , rootDir :: String
         , sagaDesc :: String
         , baseURL :: Maybe String
         , spells :: SpellDB
         , weapons :: WeaponDB
         , armour :: ArmourDB
       }  deriving (Eq)

-- | Get the most recent SagaState
-- This is mainly for backward compatibility
sagaState :: Saga -> SagaState
sagaState = head . sagaStates

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

-- | Saga state at a particular point in time, comprising characters and
-- covenants at that point.
data SagaState = SagaState 
         { stateTitle :: String
         , seasonTime :: SeasonTime
         , covenants :: [Covenant]
         , characters :: [Character]
         }  deriving (Eq,Show)

instance Timed SagaState where
    season = seasonTime

-- | Get the name of the Saga as recorded in the SagaState
sagaStateName :: SagaState -> String
sagaStateName s = stateTitle s ++ " - " ++ (show $ seasonTime s)

-- |
-- == SagaFile object

-- | A Saga as it is stored on file.
-- The main purpose here is to identify all the files used for characters and
-- other data in the saga.
data SagaFile = SagaFile 
         { title :: String
         , seasons :: [ SeasonTime ]
         , currentSeason :: SeasonTime
         , rootDirectory :: Maybe String
         , sagaDescription :: String
         , covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
         , weaponFile :: String
         , armourFile :: String
       }  deriving (Eq,Generic,Show)

instance ToJSON SagaFile 
instance FromJSON SagaFile where
    parseJSON = withObject "SagaFile" $ \v -> SagaFile
       <$> v .: "title"
       <*> v .:? "seasons" .!= []
       <*> v .:? "currentSeason" .!= NoTime
       <*> v .:? "rootDirectory" 
       <*> v .:? "description"  .!= ""
       <*> v .:? "covenantFiles" .!= []
       <*> v .:? "characterFiles" .!= []
       <*> v .:? "spellFile" .!= "spells.csv"
       <*> v .:? "weaponFile" .!= "weapons.csv"
       <*> v .:? "armourFile" .!= "armour.csv"


