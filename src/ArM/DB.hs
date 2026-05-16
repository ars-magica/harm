{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.DB
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Database types
--
--
-----------------------------------------------------------------------------
module ArM.DB ( mkArmourDB
              , mkWeaponDB
              , spellDB
              , spellLookup
              , SpellDB
              , WeaponDB
              , ArmourDB
              , ArMCSV(..)
              ) where

import ArM.DB.CSV
import qualified Data.Map as M

import ArM.Trait

type WeaponDB = M.Map String Weapon
type ArmourDB = M.Map String Armour

-- | Create a `Data.Map.Map` of Weapon objects.  
-- The input is the output from `Data.CSV.csvFile`
mkWeaponDB :: [[String]] -> WeaponDB
mkWeaponDB = M.fromList . map ( \ x -> (weaponName x,x) ) . map fromCSVline

mkArmourDB :: [[String]] -> ArmourDB
mkArmourDB = M.fromList . map ( \ x -> (armourName x,x) ) . map fromCSVline

-- | Default SpellRecord object as a starting point for step-by-step construction.
type SpellDB = M.Map String SpellRecord

-- | Create a `Data.Map.Map` of SpellRecord objects.  
-- The input is the output from `Data.CSV.csvFile`
spellDB :: [[String]] -> SpellDB
spellDB = M.fromList . map ( \ x -> (spellRecordName x,x) ) . map fromCSVline

spellLookup :: TraitKey -> SpellDB -> Maybe SpellRecord
spellLookup = M.lookup . spellKeyName
