{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Possession
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Possession including weapons and enchanted devices
--
--
-----------------------------------Types.------------------------------------------
module ArM.Types.Possession where

import ArM.Types.Calendar
import ArM.Types.HarmObject
import ArM.Types.Lab
import ArM.DB.Weapon

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KM
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad
-- import Control.Monad
import Data.Maybe

import ArM.Debug.Trace

-- |
-- == Weapons and other Possessions

-- | A `Possession` is any kind of device that can be acquired, lost,
-- given, or traded.  It is treated like inherent traits in the data
-- model.  Possessions comprise weapons, armour, vis, magic devices,
-- equipment, and any physical object that should be recorded
-- on the characters sheet.
data Possession = Possession 
     { itemName :: String            -- ^ Name identifying the unique item
     -- , itemKey :: HarmKey            -- ^ Key for a unique item
     , weaponStats :: [ Weapon ]     -- ^ List of applicable Weapon stat objects
     , weapon :: [ String ]          -- ^ List of standard weapon stats that apply
     , armourStats :: [ Armour ]     -- ^ List of applicable Weapon stat objects
     , armour :: [ String ]          -- ^ List of standard weapon stats that apply
     , enchantment :: Enchantment
     , itemDescription :: [ String ] -- ^ Description of the Item
     , itemComment :: [ String ]     -- ^ Comments, supplementing the description
     , itemArt :: Maybe String       -- ^ Relevant art if the item is raw vis
     , acTo :: Maybe String
     , itemCount :: Int              -- ^ Number of items possessed, default 1.
     , itemDate :: SeasonTime        -- ^ Time of creation
     }
     | LabPossession Lab
    deriving ( Ord, Eq, Generic )

data Enchantment = LesserItem MagicEffect
                 | GreaterDevice Int [ MagicEffect ]
                 | Talisman Int [ MagicEffect ]
                 | ChargedItem Int MagicEffect
                 | MundaneItem
    deriving ( Ord, Eq, Generic )
instance ToJSON Enchantment 

enchantmentName :: Enchantment -> String
enchantmentName (LesserItem e) = effectName e
enchantmentName (ChargedItem _ e) = effectName e
enchantmentName (GreaterDevice _ (e:_)) = effectName e
enchantmentName (Talisman _ _) = "Talisman"
enchantmentName _ = ""

{-
parseLesser :: Object -> Parser Enchantment
parseLesser = fmap LesserItem . f . KM.lookup "lesseritem"
    where f Nothing = mzero
          f (Just x) = parseJSON x
-}

parseLesser :: Object -> Parser Enchantment
parseLesser v = LesserItem
        <$> v .: "lesseritem" 

parseGreater :: Object -> Parser Enchantment
parseGreater v = GreaterDevice
        <$> v .: "viscapacity" 
        <*> v `parseCollapsedList` "effects" 
parseTalisman :: Object -> Parser Enchantment
parseTalisman v = GreaterDevice
        <$> v .: "talisman" 
        <*> v `parseCollapsedList` "effects" 
parseCharged :: Object -> Parser Enchantment
parseCharged v = ChargedItem
        <$> v .: "charged" 
        <*> v .: "effect" 

instance FromJSON Enchantment where
    parseJSON (Object v) = foldl mplus (parseLesser v) 
       [ (parseGreater v), (parseTalisman v), (parseCharged v) ]
    parseJSON _ = mzero

visArt :: Possession -> Maybe String
visArt (LabPossession _) = Nothing
visArt ob = itemArt ob
getLab :: Possession -> Maybe Lab
getLab (LabPossession lab) = Just lab
getLab _ = Nothing

isLab :: Possession -> Bool
isLab (LabPossession _) = True
isLab _ = False

specialPossession :: Possession -> Bool
specialPossession (LabPossession _) = True
specialPossession _ = False

isVis :: Possession -> Bool
isVis c | specialPossession c = False
        | otherwise = isJust $ itemArt c

isWeapon :: Possession -> Bool
isWeapon p | specialPossession p = False
           | otherwise = (weapon p /= []) || (weaponStats p /= [])

isArmour :: Possession -> Bool
isArmour p | specialPossession p = False
           | otherwise = (armour p /= []) || (armourStats p /= [])
isMagic :: Possession -> Bool
isMagic p | specialPossession p = False
       | otherwise = enchantment p /= MundaneItem
isAC :: Possession -> Bool
isAC p | specialPossession p = False
       | otherwise = isJust $ acTo p

isMundaneEquipment :: Possession -> Bool
isMundaneEquipment p | specialPossession p = False
       | otherwise = isEquipment p && (not . isMagic) p

isEquipment :: Possession -> Bool
isEquipment p = not $ foldl (||) False [ f p | f <- fs ] 
   where fs = [ isLab, isVis, isWeapon, isArmour, isAC, specialPossession ]


instance StoryObject MagicEffect where
   name ob = effectName ob 
   setName n x = x { effectName = n }
   narrative ob = effectDescription ob
   addNarrative s x = x { effectDescription = s:narrative x }
   comment ob = comment ob
   addComment s x = x { effectComment = s:comment x }

instance StoryObject Possession where
   name (LabPossession lab) = name lab
   name ob = itemName ob 
   setName _ (LabPossession _) = error "Not implemented.  LabPossession should be obsoleted."
   setName n x = x { itemName = n }
   narrative (LabPossession lab) = narrative lab
   narrative ob = itemDescription ob
   addNarrative s (LabPossession lab) = LabPossession $ addNarrative s lab
   addNarrative s x = x { itemDescription = s:narrative x }
   comment (LabPossession lab) = comment lab
   comment ob = comment ob
   addComment s (LabPossession lab) = LabPossession $ addComment s lab
   addComment s x = x { itemComment = s:comment x }

instance Countable Possession where
   count (LabPossession _) = 1
   count ob = itemCount ob
   addCount (LabPossession x) _ = trace "Labs are unique"  (LabPossession x)
   addCount ob n  = ob { itemCount = itemCount ob + n }

defaultPossession :: Possession 
defaultPossession = Possession 
     { itemName = ""
     -- , itemKey = NoObject
     , weaponStats = []
     , weapon = []
     , armourStats = []
     , armour = []
     , enchantment = MundaneItem
     , itemDescription = []
     , itemComment = []
     , itemArt = Nothing
     , acTo = Nothing
     , itemCount = 1
     , itemDate = NoTime
     }
instance ToJSON Possession 

instance FromJSON Possession where
    parseJSON (String t) = pure $ setName (unpack (fromStrict t)) defaultPossession 
    parseJSON (Object v) = (parseLab v) `mplus` (parseOtherPossession v)
    parseJSON _ = mzero


parseOtherPossession :: Object -> Parser Possession
parseOtherPossession v = fmap fixPossessionName $ Possession 
       <$> v .:? "name" .!= ""
       <*> v .:? "weaponStats" .!= []
       <*> v .:? "weapon" .!= []
       <*> v .:? "armourStats" .!= []
       <*> v .:? "armour" .!= []
       <*> v .:? "enchantment" .!= MundaneItem
       <*> v `parseCollapsedList` "description" 
       <*> v `parseCollapsedList` "comment" 
       <*> v .:? "art"
       <*> v .:? "acTo" 
       <*> v .:? "count" .!= 1
       <*> v .:? "date"  .!= NoTime

parseLab :: Object -> Parser Possession
parseLab = fmap LabPossession . f . KM.lookup "lab"
    where f Nothing = mzero
          f (Just x) = parseJSON x



-- | Derive `itemName` from other properties, if the name is undefined.
fixPossessionName :: Possession -> Possession 
fixPossessionName =  fixPN getPN1 . fixPN f
    where f p | enchantment p /= MundaneItem = enchantmentName $ enchantment p
              | otherwise = ""

fixPN :: (Possession -> String) -> Possession -> Possession 
fixPN f p | itemName p /= "" = p
          | otherwise = setName (f p) p

getPN1 :: Possession -> String 
getPN1 p | weapon p /= [] = head $ weapon p
         | armour p /= [] = head $ armour p
         | isJust (visArt p) = fromJust (visArt p) ++ " vis"
         | isAC p = "AC to " ++ (fromJust $ acTo p)
         | otherwise = "Item"

instance Show Possession where
    show p = name p ++ cnt
       where cnt | count p == 1 = ""
                 | otherwise = " (" ++ show (count p) ++ ")"



{-
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
-}

-- | A magic effect that can be instilled in an enchanted device.
data MagicEffect = MagicEffect
           { effectName :: String
           , effectLevel :: Int
           , effectTechnique :: String
           , effectTechniqueReq :: [String]
           , effectForm :: String
           , effectFormReq :: [String]
           , effectRDT :: (String,String,String)   -- ^ Range/Duration/Target
           , effectModifiers :: [ String ]
           , effectTrigger :: String
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
        <*> v .: "technique" 
        <*> v  `parseCollapsedList` "techiqueReq" 
        <*> v .: "form" 
        <*> v  `parseCollapsedList` "formReq" 
        <*> v .:? "rdt" .!= ("","","")
        <*> v `parseCollapsedList` "effectModifiers" 
        <*> v .:? "design"  .!= ""
        <*> v .:? "trigger"  .!= ""
        <*> v `parseCollapsedList` "description" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "reference"  .!= ""
        <*> v .:? "season" .!= NoTime
