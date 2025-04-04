{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Combat
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Character Traits, including Abilities, Spells, Virtues, etc..
--
-- This module proves a type for each kind of trait as well as a wrapper type,
-- `Trait` which can represent any kind of trait.
--
-- This module defines the types as well as the `TraitType` class, and instances
-- for `show`, sorting, and JSON.
--
-----------------------------------------------------------------------------------
module ArM.Char.Combat where

-- import ArM.Helper
import ArM.Char.CharacterSheet
import ArM.Char.Types.Trait
import ArM.DB.Weapon
import ArM.Helper

import GHC.Generics
import Data.Maybe
import Data.List
import qualified Data.Map as M

-- import ArM.Debug.Trace

-- |
-- = The Trait Type

data CombatLine = CombatLine 
    { combatLabel :: String
    , combatInit  :: Int
    , combatAtk   :: Maybe Int
    , combatDef   :: Maybe Int
    , combatDam   :: Maybe Int
    , combatRange :: Maybe Int
    , combatLoad  ::  Int
    , combatComment :: String
    } deriving (Show, Eq, Generic)
defaultCL :: CombatLine 
defaultCL = CombatLine 
    { combatLabel = "No label"
    , combatInit  = 0
    , combatAtk   = Nothing
    , combatDef   = Nothing
    , combatDam   = Nothing
    , combatRange = Nothing
    , combatLoad  = 0
    , combatComment = ""
    } 
data SoakLine = SoakLine 
    { soakLabel    :: String
    , soakScore    :: Int
    , soakLoad     ::  Int
    } deriving (Show, Eq, Generic)


-- | Look up weapon stats for a possession.



-- | Compute a single line for the Combat Stats table
computeCombatLine :: WeaponDB -> CharacterSheet -> CombatOption -> CombatLine
computeCombatLine db cs co
   | isNothing ab = f $ implicitAbility db cs co df
   | otherwise    = f $ explicitAbility db cs co (fromJust ab) df
     where ab = combatAbility co
           f = addCharacteristics cs 
           df = defaultCL { combatLabel = combatName co }

-- | Add characteristics to the combat stats
addCharacteristics :: CharacterSheet -> CombatLine -> CombatLine
addCharacteristics cs cl = cl
        { combatInit  = (+cqik) $ combatInit cl
        , combatAtk   = fmap (+cdex) $ combatAtk cl
        , combatDef   = fmap (+cqik) $ combatDef cl
        , combatDam   = fmap (+cstr) $ combatDam cl
        } 
   where cqik = sheetCharacteristicScore cs (CharacteristicKey "Qik")
         cdex = sheetCharacteristicScore cs (CharacteristicKey "Dex")
         cstr = sheetCharacteristicScore cs (CharacteristicKey "Str")

-- | Look up weapon stats from a possession
-- Standard stats from the DB and specific stats from the item are concatenated.
weaponStat :: WeaponDB -> Possession -> [Weapon]
weaponStat db p = ws
   where ws' = map (\ x -> M.lookup x db) $ weapon p
         ws = weaponStats p ++ filterNothing ws'

-- | Look up weapon stats from a character sheets.
-- If a possession is not found, it is looked up in the WeaponDB instead
sheetWeapon :: WeaponDB -> CharacterSheet -> String -> [ Weapon ]
sheetWeapon db cs w | ws /= [] = ws
                    | otherwise = ws'
    where ws = sheetWeapon1 db cs w
          ws' = sheetWeapon2 db w

sheetWeapon2 :: WeaponDB -> String -> [ Weapon ]
sheetWeapon2 db w | isNothing r = []
                  | otherwise = [ fromJust r ]
    where r = M.lookup w db

sheetWeapon1 :: WeaponDB -> CharacterSheet -> String -> [ Weapon ]
sheetWeapon1 db cs w = fromMaybe [] sw
    where sw' = sheetPossession cs $ PossessionKey w
          sw = fmap (weaponStat db) sw'


implicitAbility :: WeaponDB -> CharacterSheet -> CombatOption -> CombatLine -> CombatLine
implicitAbility db cs co df
  | ws == [] = df { combatComment = "No weapon" }
  | otherwise = addShield db cs ab sstr $ df
        { combatInit  = weaponInit w 
        , combatAtk   = fmap (+abscore) $ atk w 
        , combatDef   = fmap (+abscore) $ def w  
        , combatDam   = dam w 
        , combatRange = range w
        , combatLoad  = load w 
        , combatComment = absp
        } 
   where (abscore,abspec) =  sheetAbilityScore cs $ AbilityKey ab
         absp | isNothing abspec = ""
              | otherwise = "Speciality " ++ fromJust abspec
         wstr = combatWeapon co
         sstr = combatShield co
         ws = sheetWeapon db cs wstr
         w:_ = ws
         ab = weaponAbility w

addShield :: WeaponDB -> CharacterSheet -> String -> Maybe String -> CombatLine -> CombatLine
addShield _ _ _ Nothing df = df
addShield db cs ab (Just sstr) df 
    | isNothing sh' = df { combatComment = "Shield not found" }
    | otherwise = df 
    { combatInit  = combatInit df + weaponInit sh
    , combatAtk   = fmap (+( fromMaybe 0 $ atk sh)) $ combatAtk df
    , combatDef   = fmap (+( fromMaybe 0 $ def sh)) $ combatDef df
    , combatDam   = fmap (+( fromMaybe 0 $ dam sh) ) $ combatDam df
    , combatLoad  = combatLoad df + load sh
    } 
    where shs = sheetWeapon db cs sstr
          sh' = find ( (ab==) . weaponAbility ) shs
          sh = fromJust sh'
explicitAbility :: WeaponDB -> CharacterSheet -> CombatOption -> String -> CombatLine -> CombatLine
explicitAbility db cs co ab df
  | isNothing w' = df { combatComment = "No weapon" }
  | otherwise = addShield db cs ab sstr $ df
        { combatInit  = weaponInit w 
        , combatAtk   = fmap (+abscore) $ atk w 
        , combatDef   = fmap (+abscore) $ def w  
        , combatDam   = dam w 
        , combatRange = range w
        , combatLoad  = load w 
        , combatComment = absp
        } 
   where (abscore,abspec) = sheetAbilityScore cs $ AbilityKey ab
         absp | isNothing abspec = ""
              | otherwise = "Speciality " ++ fromJust abspec
         wstr = combatWeapon co
         sstr = combatShield co
         ws = sheetWeapon db cs wstr
         w' = find ( (ab==) . weaponAbility ) ws
         w =  fromJust w'

-- | Collapse a neste Maybe Maybe object to a single Maybe
join :: Maybe (Maybe a) -> Maybe a
join Nothing = Nothing
join (Just Nothing) = Nothing
join (Just (Just x)) = Just x

-- | Compute the table of combat stas from a list of `CombatOption` objects
computeCombatLines :: WeaponDB -> CharacterSheet -> [CombatOption] -> [CombatLine]
computeCombatLines db sh = map (computeCombatLine db sh)

-- | Compute the table of combat stas from a list of `CharacterSheet` objects
computeCombatStats :: WeaponDB -> CharacterSheet -> [CombatLine]
computeCombatStats db sh = computeCombatLines db sh $ combatList sh
