{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Device
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Enchanted Devices
--
--
-----------------------------------Types.------------------------------------------
module ArM.Types.Device where

import ArM.Types.Calendar
import ArM.Types.HarmObject
-- import ArM.Debug.Trace

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
-- import Data.Aeson.Types
-- import Control.Monad
-- import Data.Maybe

-- |
-- = The Trait Type

data EnchantmentType = LesserItem | GreaterDevice | ChargedItem | Talisman
           deriving (Show, Eq, Ord, Generic)

instance ToJSON EnchantmentType
instance FromJSON EnchantmentType 

data MagicItem = MagicItem
           { deviceName :: String
           , enchantmentType :: EnchantmentType
           , deviceVis :: Int
           , effect :: [ MagicEffect ]
           , deviceDate :: SeasonTime   -- ^ Time the device was first crafted 
           , deviceDescription :: [String]
           , deviceComment :: String    -- ^ Freeform remarks that do not fit elsewhere
           }
           deriving (Show, Eq, Ord, Generic)

instance ToJSON MagicItem
instance FromJSON MagicItem where
    parseJSON = withObject "MagicItem" $ \v -> MagicItem
        <$> v .: "name" 
        <*> v .:? "type" .!= LesserItem
        <*> v .:? "visSlots" .!= 0
        <*> v `parseCollapsedList` "effect" 
        <*> v .:? "season" .!= NoTime
        <*> v `parseCollapsedList` "description" 
        <*> v `parseCollapsedList` "comment" 

data MagicEffect = MagicEffect
           { effectName :: String
           , effectLevel :: Int
           , effectTechnique :: String
           , effectTechniqueReq :: [String]
           , effectForm :: String
           , effectFormReq :: [String]
           , effectRDT :: (String,String,String)   -- ^ Range/Duration/Target
           , effectModifiers :: [ String ]
           , effectDesign :: String     -- ^ Level calculation
           , effectDescription :: [String]
           , effectComment :: [String]    -- ^ Freeform remarks that do not fit elsewhere
           , effectReference :: String  -- ^ Source reference
           , effectDate :: SeasonTime   -- ^ Time of investment
           }
           deriving (Show, Eq, Ord, Generic)


instance ToJSON MagicEffect
instance FromJSON MagicEffect where
    parseJSON = withObject "MagicEffect" $ \v -> MagicEffect
        <$> v .: "name" 
        <*> v .: "level" 
        <*> v .: "techique" 
        <*> v  `parseCollapsedList` "techiqueReq" 
        <*> v .: "form" 
        <*> v  `parseCollapsedList` "formReq" 
        <*> v .:? "rdt" .!= ("","","")
        <*> v `parseCollapsedList` "effectModifiers" 
        <*> v .:? "design"  .!= ""
        <*> v `parseCollapsedList` "description" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "reference"  .!= ""
        <*> v .:? "season" .!= NoTime
