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
import Data.Aeson.Extra
import Data.Maybe
import Data.List

import ArM.Helper
import ArM.Types.HarmObject
import ArM.Types.Calendar
import ArM.Types.Story

import ArM.Debug.Trace


data Lab = Lab 
         { labName :: String
         , labDescription :: String
         , labState :: LabState
         -- , pastLabAdvancement :: [ LabAdvancement ]
         -- , futureLabAdvancement :: [ LabAdvancement ]
       }  deriving (Eq,Generic,Show)

instance ToJSON Lab
instance FromJSON Lab where
    parseJSON = withObject "Lab" $ \v -> Lab
        <$> v .: "name"
        <*> v .:? "description" .!= ""
        <*> v .:? "state" .!= defaultLabState
        -- <*> v .:? "history" .!= []
        -- <*> v .:? "plan" .!= []

instance KeyObject Lab where
    harmKey = LabKey . labName

instance Timed Lab where
    season = labTime . labState
instance HarmObject Lab 
instance StoryObject Lab where
    name = labName 

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
     , labVirtueComment :: [ String ]
     }  deriving (Eq,Generic,Show)

instance ToJSON LabVirtue
instance FromJSON LabVirtue where
    parseJSON = withObject "LabVirtue" $ \v -> LabVirtue
        <$> v .: "name" 
        <*> v .:? "detail" .!= ""
        <*> v .:? "description" .!= ""
        <*> v .:? "bonus" .!= []
        <*> v .:? "mechanics" .!= ""
        <*> v `parseCollapsedList` "comment" 

data LabBonus = LabBonus 
     { labTrait :: String
     , labSpecialisation :: String
     , labScore :: Int
     }  deriving (Eq,Generic,Show)


(<%) :: LabBonus -> LabBonus -> Bool
(<%) a b | labTrait a < labTrait b = True
         | labTrait b < labTrait a = False
         | labSpecialisation a < labTrait b = True
         | labSpecialisation b < labTrait a = False
	 | otherwise = False
lbOrd :: LabBonus -> LabBonus -> Ordering
lbOrd a b | a <% b = LT
          | b <% a = GT
          | otherwise = EQ

instance ToJSON LabBonus
instance FromJSON LabBonus where
    parseJSON = withObject "LabBonus" $ \v -> LabBonus
        <$> v .: "name" 
        <*> v .:? "specialisation" .!= ""
        <*> v .:? "score" .!= 0

mergeBonus :: [ LabBonus ] -> [ LabBonus ] -> [ LabBonus ]
mergeBonus [] ys = ys
mergeBonus xs [] = xs
mergeBonus (x:xs) (y:ys) | x <% y = x:mergeBonus xs (y:ys)
                         | y <% x = y:mergeBonus (x:xs) ys
                         | otherwise = f x y:mergeBonus xs ys
    where f x y = x { labScore = labScore x + labScore y }

totalBonus :: [ LabVirtue ] -> [ LabBonus ]
totalBonus = foldl mergeBonus [] . map (sortBy lbOrd . labVirtueBonus)

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
     , newSize = Nothing
     , labRefine = 0
     }
