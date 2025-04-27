{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Lab
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent the laboratory and sanctum of a magus
--
--
-----------------------------------------------------------------------------
module ArM.Types.Lab where

import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.List

import ArM.Helper
import ArM.Types.HarmObject

import ArM.Debug.Trace


data Lab = Lab 
         { labName :: String
         , labDescription :: String
	 , labState :: LabState
         , pastLabAdvancement :: [ LabAdvancement ]
         , futureLabAdvancement :: [ LabAdvancement ]
       }  deriving (Eq,Generic,Show)
instance ToJSON Lab
instance FromJSON Lab
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "name"
        <*> v .:? "description" .!= ""
        <*> v .:? "state" .!= defaultLabState
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []

instance KeyObject Lab where
    harmKey = CovenantKey . labName

instance Timed Lab where
    season = fromMaybe NoTime . fmap labTime . labState
instance HarmObject Lab where
    name = labName 
    prepare x = id

-- |
-- = LabState Object

data LabState = LabState 
         { labTime :: SeasonTime
         , labVirtues :: [ LabVirtue ]
         , labRefinement :: Int 
         , labSize :: Int 
       }  deriving (Eq,Generic,Show)

defaultLabState :: LabState 
defaultLabState = LabState 
         { labTime = GameStart
         , labVirtues = []
         , labRefinement = 0
         , labSize = 0
       }  


instance ToJSON LabState
instance FromJSON LabState where
    parseJSON = withObject "LabState" $ \v -> LabState
        <$> v .:? "season" .!= GameStart
        <*> v .:? "virtues" .!= []
        <*> v .:? "refinement" .!= 0
        <*> v .:? "size" .!= 0

data LabVirtue = LabVirtue 
     { labVirtueName :: String
     , labVirtueDetail :: String
     , labVirtueDescription :: String
     , labVirtueBonus :: [ LabBonus ]
     , labVirtueMechanics :: String
     }  deriving (Eq,Generic,Show)

instance ToJSON LabVirtue
instance FromJSON LabVirtue where
    parseJSON = withObject "LabVirtue" $ \v -> LabVirtue
        <$> v .: "name" 
        <*> v .:? "detail" .!= ""
        <*> v .:? "description" .!= ""
        <*> v .:? "bonus" .!= []
        <*> v .:? "mechanics" .!= ""

data LabBonus = LabBonus 
     { labTrait :: String
     , labSpecialisation :: String
     , labScore :: Int
     }  deriving (Eq,Generic,Show)

instance ToJSON LabBonus
instance FromJSON LabBonus where
    parseJSON = withObject "LabBonus" $ \v -> LabBonus
        <$> v .: "name" 
        <*> v .:? "specialisation" .!= ""
        <*> v .:? "score" .!= 0

-- |
-- = Advancement and Traits

-- | Advancement (changes) to a covenant.
data LabAdvancement = LabAdvancement 
     { labSeason :: SeasonTime    -- ^ season or development stage
     , labNarrative :: [String]   -- ^ freeform description of the activities
     , acquireVirtue :: [ LabVirtue ]
     , loseVirtue :: [ LabVirtue ]
     , newSize :: Maybe Int
     , labRefine :: Int
     }
   deriving (Eq,Generic,Show)
defaultAdv :: LabAdvancement 
defaultAdv = LabAdvancement 
     { labSeason = NoTime
     , labNarrative = []
     , acquireVirtue = []
     , loseVirtue = []
     , newSize = Nothung
     , labRefine = 0
     }
