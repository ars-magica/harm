{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Harm.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Types.Harm.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
                    , sagaDesc
                    , rootDir
                    , stateSeasons
                    , advSeasons
                    ) where


import Data.Maybe 
import Data.List 
import Data.Aeson 
import GHC.Generics

import ArM.Story
import ArM.Types.Harm.Covenant
import ArM.Types.Harm.Character
import ArM.DB

-- |
-- = Saga type

-- | A Saga as it is processed in memory.
-- The Saga object includes covenant and character objects.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaFile :: SagaFile
         , sagaState :: SagaState
         , baseURL :: Maybe String
         , spells :: SpellDB
         , weaponsDB :: WeaponDB
         , armourDB :: ArmourDB
       }  deriving (Eq)

-- | Return the description of the saga, as stored in the `SagaFile` object.
sagaDesc :: Saga -> [ String ]
sagaDesc = sagaDescription . sagaFile
-- | Return the saga title, as stored in the `SagaFile` object.
sagaTitle :: Saga -> String
sagaTitle = title . sagaFile
-- | Return root directory, as stored in the `SagaFile` object.
rootDir :: Saga -> String
rootDir = fromMaybe "" . rootDirectory . sagaFile

-- | Get the seasons for which character states should be generated.
-- The seasons are listed chronologically with the latest season first.
stateSeasons :: Saga -> [ SeasonTime ]
stateSeasons = reverse . (GameStart:) . seasons . sagaFile

-- | List of the last season of advancement for each state output.
advSeasons :: Saga -> [SeasonTime]
advSeasons = map seasonPrev . sort . seasons . sagaFile 

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

instance Timed Saga where
    season = seasonTime . sagaState
instance StoryObject Saga where
    name = sagaTitle
    narrative = sagaDesc
instance HarmObject Saga where
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


