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
                    , sagaDesc
                    , rootDir
                    , stateSeasons
                    ) where


import Data.Maybe 
import Data.Aeson 
-- import Data.List 
import GHC.Generics

import ArM.Types
import ArM.Types.Covenant
import ArM.Types.Character
import ArM.DB.Spell
import ArM.DB.Weapon

-- |
-- = Saga type

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaFile :: SagaFile
         , sagaState :: SagaState
         , baseURL :: Maybe String
         , spells :: SpellDB
         , weapons :: WeaponDB
         , armour :: ArmourDB
       }  deriving (Eq)

sagaDesc :: Saga -> [ String ]
sagaDesc = sagaDescription . sagaFile
sagaTitle :: Saga -> String
sagaTitle = title . sagaFile
rootDir :: Saga -> String
rootDir = fromMaybe "" . rootDirectory . sagaFile

stateSeasons :: Saga -> [ SeasonTime ]
stateSeasons = reverse . (GameStart:) . seasons . sagaFile

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

instance Timed Saga where
    season = seasonTime . sagaState
instance HarmObject Saga where
    name = sagaTitle
    stateName s = name s ++ " - " ++ (show $ season $ sagaState s)

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
         , sagaDescription :: [String]
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
       <*> v .:? "description"  .!= []
       <*> v .:? "covenantFiles" .!= []
       <*> v .:? "characterFiles" .!= []
       <*> v .:? "spellFile" .!= "spells.csv"
       <*> v .:? "weaponFile" .!= "weapons.csv"
       <*> v .:? "armourFile" .!= "armour.csv"


