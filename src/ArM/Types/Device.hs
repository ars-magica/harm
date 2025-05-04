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

import ArM.GameRules
import ArM.Helper
import ArM.Types.TraitKey
import ArM.Types.HarmObject
import ArM.Types.Lab
import ArM.Types.Aging
import ArM.DB.Weapon
-- import ArM.Debug.Trace

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KM
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad
import Data.Maybe
import Data.List (sortBy)

-- |
-- = The Trait Type

data EnchantmentType = LesserItem | GreaterDevice | ChargedItem | Talisman

data MagicItem = MagicItem
           { enchantmentType :: EnchantmentType
	   , name :: String
	   }
           deriving (Show, Eq, Generic)
