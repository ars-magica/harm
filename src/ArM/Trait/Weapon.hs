{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait.Weapon
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types for Weapon and Armour tables
--
--
-----------------------------------------------------------------------------
module ArM.Trait.Weapon where

import GHC.Generics
import Data.Aeson

data Weapon = Weapon
    { weaponName :: String
    , weaponAbility :: String
    , weaponInit :: Int
    , atk :: Maybe Int
    , def :: Maybe Int
    , dam :: Maybe Int
    , str :: Maybe Int
    , range :: Maybe Int
    , load ::  Int
    , weaponCost :: String
    } deriving ( Show, Ord, Eq, Generic )
data Armour = Armour
    { armourName :: String
    , armourLoad :: Int
    , armourProtection :: Int
    , armourCost :: String
    } deriving ( Show, Ord, Eq, Generic )

instance FromJSON Weapon 
instance ToJSON Weapon 
instance FromJSON Armour 
instance ToJSON Armour 

