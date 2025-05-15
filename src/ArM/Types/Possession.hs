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
-- import ArM.Types.Lab
import ArM.Types.Library
import ArM.DB.Weapon
import ArM.Helper

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.Types
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad
import Data.Maybe

-- import ArM.Debug.Trace

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
     , bookTexts :: [ Book ]         -- ^ List of included texts, if the item is a Book
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
visArt = itemArt 

isVis :: Possession -> Bool
isVis c = isJust $ itemArt c

isBook :: Possession -> Bool
isBook p = bookTexts p /= []

isWeapon :: Possession -> Bool
isWeapon p = (weapon p /= []) || (weaponStats p /= [])

isArmour :: Possession -> Bool
isArmour p = (armour p /= []) || (armourStats p /= [])
isMagic :: Possession -> Bool
isMagic p = enchantment p /= MundaneItem
isAC :: Possession -> Bool
isAC p = isJust $ acTo p

isMundaneEquipment :: Possession -> Bool
isMundaneEquipment p = isEquipment p && (not . isMagic) p

isEquipment :: Possession -> Bool
isEquipment p = not $ foldl (||) False [ f p | f <- fs ] 
   where fs = [ isVis, isWeapon, isArmour, isAC ]


instance StoryObject MagicEffect where
   name ob = effectName ob 
   setName n x = x { effectName = n }
   narrative ob = effectDescription ob
   addNarrative s x = x { effectDescription = s:narrative x }
   comment ob = effectComment ob
   addComment s x = x { effectComment = s:comment x }

instance StoryObject Possession where
   name ob = itemName ob 
   setName n x = x { itemName = n }
   narrative ob = itemDescription ob
   addNarrative s x = x { itemDescription = s:narrative x }
   comment ob = comment ob
   addComment s x = x { itemComment = s:comment x }

instance Countable Possession where
   count ob = itemCount ob
   addCount ob n  = ob { itemCount = itemCount ob + n }

defaultPossession :: Possession 
defaultPossession = Possession 
     { itemName = ""
     -- , itemKey = NoObject
     , bookTexts = []
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
    parseJSON (Object v) = (parseOtherPossession v)
    parseJSON _ = mzero


parseOtherPossession :: Object -> Parser Possession
parseOtherPossession v = fmap fixPossessionName $ Possession 
       <$> v .:? "name" .!= ""
       <*> v `parseCollapsedList` "texts" 
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



-- | A magic effect that can be instilled in an enchanted device.
data MagicEffect = MagicEffect
           { effectName :: String
           , effectLevel :: Int
           , effectTechnique :: String
           , effectTechniqueReq :: [String]
           , effectForm :: String
           , effectFormReq :: [String]
           , effectRange :: String        -- ^ Range
           , effectDuration :: String     -- ^ Duration
           , effectTarget :: String       -- ^ Target
           , effectModifiers :: [ String ]
           , effectTrigger :: String
           , effectDesign :: String     -- ^ Level calculation
           , effectDescription :: [String]
           , effectComment :: [String]    -- ^ Freeform remarks that do not fit elsewhere
           , effectReference :: String  -- ^ Source reference
           , effectDate :: SeasonTime   -- ^ Time of investment
           }
           deriving (Show, Eq, Ord, Generic)

effectRDT :: MagicEffect -> String
effectRDT eff = showStrList [ r, d, t ]
   where r = f "Range" (effectRange eff)
         d = f "Duration" (effectDuration eff)
         t = f "Target" (effectTarget eff)
         f _ "" = ""
         f s x = s ++ ": " ++ x



instance ToJSON MagicEffect
instance FromJSON MagicEffect where
    parseJSON = withObject "MagicEffect" $ \v -> MagicEffect
        <$> v .: "name" 
        <*> v .: "level" 
        <*> v .: "technique" 
        <*> v  `parseCollapsedList` "techiqueReq" 
        <*> v .: "form" 
        <*> v  `parseCollapsedList` "formReq" 
        <*> v .:? "range" .!= ""
        <*> v .:? "duration" .!= ""
        <*> v .:? "target" .!= ""
        <*> v `parseCollapsedList` "modifiers" 
        <*> v .:? "trigger"  .!= ""
        <*> v .:? "design"  .!= ""
        <*> v `parseCollapsedList` "description" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "reference"  .!= ""
        <*> v .:? "season" .!= NoTime
