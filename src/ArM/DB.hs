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
              , readYaml
              , readDB
              ) where

import qualified Data.Map as M
import Data.Aeson (FromJSON)
import Data.Aeson.Generic (readObject)
-- import Data.Maybe
import System.FilePath

import ArM.DB.CSV
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

-- | Load a DB from a YAML/JSON file.
readYaml :: (ArMCSV t,FromJSON t) 
         => String -- ^ Filename
            -> IO ( Maybe ( M.Map String t ) )
readYaml fn = readObject fn >>= return . fmap mkDB 
   where mkDB = M.fromList . map ( \ x -> (getID x,x) ) 

-- | Load a DB from either YAML/JSON or CSV.
--
-- If he filename ends in ".csv", CSV is assumed.
-- Otherwise the file has to be in either YAML or JSON format.
readDB :: (ArMCSV t,FromJSON t) 
         => String -- ^ Filename
            -> IO ( M.Map String t )
readDB f = readDB' (takeExtension f) f
   where readDB' ".csv" = fmap fm . readCSV
         readDB' _ = fmap fm . readYaml
         fm Nothing = error ("Failed to read DB file: " ++ f)
         fm (Just x) = x
