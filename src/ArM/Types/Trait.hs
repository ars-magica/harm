{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Trait
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
-----------------------------------Types.------------------------------------------
module ArM.Types.Trait ( TraitKey(..)
                            , Trait(..)
                            , Ability(..)
                            , Characteristic(..)
                            , Art(..)
                            , VF(..)
                            , Spell(..)
                            , PTrait(..)
                            , SpecialTrait(..)
                            , OtherTrait(..)
                            , Possession(..)
                            , CombatOption(..)
                            , Confidence(..)
                            , Reputation(..)
                            , Age(..)
                            , TraitClass(..)
                            , (<:)
                            , fote
                            , sortTraits
                            , visArt
                            , getLab
                            , isVis
                            , isWeapon
                            , isArmour
                            , isAC
                            , isEquipment
                            ) where

import ArM.GameRules
import ArM.Helper
import ArM.Types.TraitKey
import ArM.Types.HarmObject
import ArM.Types.Lab
import ArM.Types.Device
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

data Trait = AbilityTrait Ability
           | CharacteristicTrait Characteristic
           | ArtTrait Art
           | SpellTrait Spell
           | PTraitTrait PTrait
           | ReputationTrait Reputation
           | VFTrait VF
           | ConfidenceTrait Confidence
           | OtherTraitTrait OtherTrait
           | SpecialTraitTrait SpecialTrait
           | PossessionTrait Possession
           | CombatOptionTrait CombatOption
           | AgeTrait Age
           deriving (Show, Eq, Generic)
instance Ord Trait where
     compare x y = compare (traitKey x) (traitKey y)

-- |
-- = Different types of Traits

data Ability = Ability { abilityName :: String
                       , speciality :: Maybe String
                       , abilityXP :: XPType 
                       , abilityScore :: Int 
                       , abilityBonus :: Int 
                       , abilityMultiplier :: Float
                       , abilityExcessXP :: XPType 
                       }
           deriving (Ord, Eq, Generic)
data Characteristic = Characteristic { characteristicName :: String
                                     , charScore :: Int
                                     , agingPoints :: Int 
                                     , charBonusList :: [(Int,Int)] 
                                     }
           deriving (Eq, Generic)
instance Ord Characteristic where
     compare x y = compare (traitKey x) (traitKey y)
data Art = Art { artName :: String
               , artXP :: XPType 
               , artScore :: Int 
               , artBonus :: Int 
               , artMultiplier :: Float
               , artExcessXP :: XPType 
               }
           deriving (Eq, Generic)
instance Ord Art where
     compare x y = compare (traitKey x) (traitKey y)

data Spell = Spell { spellName :: String
                   , spellTeFo :: String
                   , spellLevel :: Int
                   , spellXP :: XPType
                   , masteryScore :: Int
                   , spellExcessXP :: XPType
                   , spellMultiplier :: Float
                   , masteryOptions :: [String] 
                   , spellCastingScore :: Maybe Int
                   , spellTComment :: String
                   -- , spellRecord :: Maybe SpellRecord
                   }
           deriving (Ord, Eq, Generic)

{-
-- | Return a string of Technique/Form/Level classifying the Spell.
spellTeFoLe :: Spell -> String
spellTeFoLe sp = spellTeFo sp ++ show (spellLevel sp)
-}

-- | Return a string of Form/Technique for sorting
spellFoTe :: Spell -> String
spellFoTe = fote . spellTeFo 

-- | Convert the TeFo string to Form/Technique for sorting
fote :: String -> String
fote tf = drop 2 tf ++ take 2 tf

-- | Personality trait
data PTrait = PTrait { ptraitName :: String, pscore :: Int }
           deriving (Ord, Eq, Generic)

-- | Reputation object 
data Reputation = Reputation { reputationName :: String  -- ^ contents of the reputation
                             , repLocale :: String       -- ^ domain or location of the reputation
                             ,  repXP :: XPType          -- ^ total XP in the reputation (used?)
                             ,  repScore :: Int          -- ^ reputation Score
                             ,  repExcessXP :: XPType    -- ^ XP towards next level in the reputation
                             }
           deriving (Ord, Eq, Generic)
data VF = VF { vfname :: String    -- ^ name of the virtue/flaw
             , vfDetail :: String  -- ^ detail, where the virtue/flaw has options
             , vfcost :: Int       -- ^ cost, should be zero for free/inferred virtues/flaws
             , vfAppliesTo :: Maybe TraitKey  -- ^ not used
             , vfMultiplicity :: Int          -- ^ number of times the virtue/flaw is take
             , vfComment :: String              -- ^ freeform comment
             }
           deriving (Ord, Eq, Generic)
data Confidence = Confidence { cname :: String, cscore :: Int, cpoints :: Int }
           deriving ( Ord, Eq, Generic)
data OtherTrait = OtherTrait { trait :: String
                             , otherScore :: Int
                             -- , pts :: Int 
                             , otherExcess :: Int
                             }
           deriving (Ord, Eq, Generic)
data SpecialTrait = SpecialTrait { specialTrait :: String
                             , specialScore :: Maybe Int
                             , specialComment :: Maybe String 
                             }
           deriving (Show, Ord, Eq, Generic)
instance FromJSON SpecialTrait
instance ToJSON SpecialTrait


-- |
-- == Show instances

instance Show VF  where
   show a = vfname a ++ f sp ++ " (" ++ cst ++ ")"
      where sp = vfDetail a
            f "" = ""
            f x = " [" ++ x ++ "]"
            cst | m == 1 = show (vfcost a) 
                | otherwise = show (vfcost a) ++ "x" ++ show m
            m = vfMultiplicity a
instance Show Confidence  where
   show a = cname a ++ ": " ++ show (cscore a) ++ " (" ++ show (cpoints a) ++ ")"
instance Show OtherTrait  where
   show a = trait a ++ ": " ++ show (otherScore a) ++ " (" ++ show (otherExcess a) ++ ")"
instance Show PTrait  where
   show a = ptraitName a ++ " " ++ show (pscore a)
instance Show Ability  where
   show a = abilityName a ++ " [" ++ showspec sp ++ "] "
          ++ show (abilityScore a) 
          ++ showBonus (abilityBonus a)
          ++ " (" ++ showNum (abilityExcessXP a) ++ "xp)"
          ++ f (abilityMultiplier a)
      where showspec Nothing = "  --  "
            showspec (Just s) = s
            sp = speciality a
            f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Characteristic  where
   show a = characteristicName a ++ " " ++ showSigned (charScore a)
          ++ showA (agingPoints a)
       where showA x | x == 0 = ""
                    | otherwise = " (" ++ show x ++ " aging points)"
instance Show Spell  where
   show a = "*" ++ spellName a ++ "* " 
            ++ spellTeFo a ++ show (spellLevel a) ++ f (spellCastingScore a)
      where f Nothing = ""
            f (Just x) = " (" ++ show x ++ ")"
instance Show Art  where
   show a = artName a ++ " " 
          ++ show (artScore a) 
          ++ showBonus (artBonus a)
          ++ " (" ++ showNum (artExcessXP a) ++ "xp) "
          ++ f (artMultiplier a)
      where f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Reputation where
   show a = reputationName a ++ " [" ++ (repLocale a) ++ "] "
          ++ show (repScore a) ++ " (" ++ showNum (repExcessXP a) ++ ") "

-- |
-- == Combat Options

-- | A CombatOption is a combination of weapons for which to list combat stats.
--
-- It needs to link with a `Weapon` (and optionally a shield) which may be
-- generic or unique.  If the weapon can be used in different modes, the
-- ability has to be linked as well.
data CombatOption = CombatOption 
     { combatName :: String          -- ^ Describing Name of the weapon combination
     , combatWeapon :: String        -- ^ The main weapon
     , combatShield :: Maybe String  -- ^ A Shield is optional
     , combatAbility :: Maybe String
     }  deriving (Eq,Ord,Generic)

instance Show CombatOption where
   show co = "Combat Option: " ++ combatName co ++ ab ++ " " ++ (combatWeapon co) ++ sh
      where ab | isNothing (combatAbility co) = ""
               | otherwise = " (" ++ (fromJust $ combatAbility co) ++ ")"
            sh | isNothing (combatShield co) = ""
               | otherwise = "/" ++ (fromJust $ combatShield co) 

instance ToJSON CombatOption
instance FromJSON CombatOption where
    parseJSON = fmap f . parseJSON'
      where f p | combatName p == "" = p { combatName = combatWeapon p }
                | otherwise = p
            parseJSON' = withObject "CombatOption" $ \v -> CombatOption
                    <$> v .:?  "name" .!= ""
                    <*> v .:   "weapon"
                    <*> v .:?  "shield"
                    <*> v .:?  "ability"
-- |
-- == Weapons and other Possessions

-- | A `Possession` is any kind of device that can be acquired, lost,
-- given, or traded.  It is treated like inherent traits in the data
-- model.  Possessions comprise weapons, armour, vis, magic devices,
-- equipment, and any physical object that should be recorded
-- on the characters sheet.
data Possession = Possession 
     { itemName :: String           -- ^ Name identifying the unique item
     , itemKey :: HarmKey           -- ^ Key for a unique item
     , weaponStats :: [ Weapon ]    -- ^ List of applicable Weapon stat objects
     , weapon :: [ String ]         -- ^ List of standard weapon stats that apply
     , armourStats :: [ Armour ]    -- ^ List of applicable Weapon stat objects
     , armour :: [ String ]         -- ^ List of standard weapon stats that apply
     , itemDescription :: String    -- ^ Description of the Item
     , itemArt :: Maybe String       -- ^ Relevant art if the item is raw vis
     , acTo :: Maybe String
     , itemCount :: Int             -- ^ Number of items possessed, default 1.
     }
     | DevicePossession MagicItem
     | LabPossession Lab
    deriving ( Ord, Eq, Generic )
visArt :: Possession -> Maybe String
visArt (LabPossession _) = Nothing
visArt (DevicePossession _) = Nothing
visArt ob = itemArt ob
getLab :: Possession -> Maybe Lab
getLab (LabPossession lab) = Just lab
getLab _ = Nothing

isLab :: Possession -> Bool
isLab (LabPossession _) = True
isLab _ = False

specialPossession :: Possession -> Bool
specialPossession (LabPossession _) = True
specialPossession (DevicePossession _) = True
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
isAC :: Possession -> Bool
isAC p | specialPossession p = False
       | otherwise = isJust $ acTo p

isEquipment :: Possession -> Bool
isEquipment p = not $ foldl (||) False [ f p | f <- fs ] 
   where fs = [ isLab, isVis, isWeapon, isArmour, isAC, specialPossession ]


instance StoryObject Possession where
   name (LabPossession lab) = name lab
   name ob = itemName ob 
   narrative (LabPossession lab) = narrative lab
   narrative ob = [ itemDescription ob ]
   comment (LabPossession lab) = comment lab
   comment _ = [ ]

instance Countable Possession where
   count (LabPossession _) = 1
   count ob = itemCount ob
   addCount (LabPossession _) _ = error "Labs are unique"
   addCount (DevicePossession _) _ = error "Magic devices are unique"
   addCount ob n  = ob { itemCount = itemCount ob + n }

defaultPossession :: Possession 
defaultPossession = Possession 
     { itemName = ""
     , itemKey = NoObject
     , weaponStats = []
     , weapon = []
     , armourStats = []
     , armour = []
     , itemDescription = ""
     , itemArt = Nothing
     , acTo = Nothing
     , itemCount = 1
     }
instance ToJSON Possession 

instance FromJSON Possession where
    parseJSON (String t) = pure $ defaultPossession { itemName = (unpack (fromStrict t)) }
    parseJSON (Object v) = (parseLab v) `mplus` (parseOtherPossession v)
    parseJSON _ = mzero


parseOtherPossession :: Object -> Parser Possession
parseOtherPossession v = fmap fixPossessionName $ Possession 
       <$> v .:? "name" .!= ""
       <*> v .:? "weaponStats" .!= NoObject
       <*> v .:? "weaponStats" .!= []
       <*> v .:? "weapon" .!= []
       <*> v .:? "armourStats" .!= []
       <*> v .:? "armour" .!= []
       <*> v .:? "description" .!= ""
       <*> v .:? "art"
       <*> v .:? "acTo" 
       <*> v .:? "count" .!= 1

parseLab :: Object -> Parser Possession
parseLab = fmap LabPossession . f . KM.lookup "lab"
    where f Nothing = mzero
          f (Just x) = parseJSON x

{-
instance FromJSON Request where
  parseJSON (Object v) =
    case H.lookup "req1" v of
      Just (Object h) -> Req1 <$> h .: "id" <*> h .: "properties"
      Nothing ->
        case H.lookup "req2" v of
          Just (Object h) -> Req2 <$> h .: "id" <*> h .: "properies"
          Nothing ->
            case H.lookup "req3" v of
              Just (Object h) -> Req3 <$> h .: "id" <*> h .: "time"
              Nothing -> mzero
-}

-- | Derive `itemName` from other properties, if the name is undefined.
fixPossessionName :: Possession -> Possession 
fixPossessionName p | itemName p /= "" = p
                    | otherwise = p { itemName = n }
            where n | weapon p /= [] = head $ weapon p
                    | armour p /= [] = head $ armour p
                    | isJust (visArt p) = fromJust (visArt p) ++ " vis"
                    | isAC p = "AC to " ++ (fromJust $ acTo p)
                    | otherwise = "Item"

instance Show Possession where
    show p = name p ++ cnt
       where cnt | count p == 1 = ""
                 | otherwise = " (" ++ show (count p) ++ ")"

-- |
-- = TraitClass 

-- | `TraitClass` provides the functions to get the search key (TraitKey),
-- to wrap and unwrap traits in the generic `Trait` type, and to filter
-- traits of different types.
-- 
-- `ProtoTrait` and its constituent types may also implement TraitClass
-- but `getTrait` may then always return Nothing.
class TraitClass t where
    -- | Get the key of the trait
    traitKey :: t -> TraitKey
    -- | Wrap the trait as a generic `Trait` object.
    toTrait :: t -> Trait
    -- | Return the specific trait from the generic Trait,
    -- or Nothing if the type does not match.
    getTrait :: Trait -> Maybe t

    -- | Extract traits of the given type from a generic list of Trait objects.
    -- It returns a pair of lists with the selected traits in the first list
    -- and the remaining traits in the other.
    filterTrait :: [ Trait ] -> ( [ t ], [ Trait ] )
    filterTrait ts = y where (_,y) = filterTrait' (ts,([],[]))

    -- | Recursive helper for `filterTrait`
    filterTrait' :: ( [ Trait ], ( [ t ], [ Trait ] ) )
                  -> ( [ Trait ], ( [ t ], [ Trait ] ) )
    filterTrait' ([],y) = ([],y)
    filterTrait' (x:xs,(ys,zs)) | isNothing ab  = filterTrait' (xs,(ys,x:zs))
                                | otherwise = filterTrait' (xs,(fromJust ab:ys,zs))
        where ab = getTrait x


instance TraitClass Trait where
    traitKey (CharacteristicTrait x) = traitKey x
    traitKey (AbilityTrait x) = traitKey x
    traitKey (ArtTrait x) = traitKey x
    traitKey (SpellTrait x) = traitKey x
    traitKey (ReputationTrait x) = traitKey x
    traitKey (VFTrait x) = traitKey x
    traitKey (PTraitTrait x) = traitKey x
    traitKey (OtherTraitTrait x) = traitKey x
    traitKey (ConfidenceTrait x) = traitKey x
    traitKey (SpecialTraitTrait x) = traitKey x
    traitKey (PossessionTrait x) = traitKey x
    traitKey (CombatOptionTrait x) = traitKey x
    traitKey (AgeTrait x) = traitKey x
    toTrait = id
    getTrait = Just . id

instance TraitClass Ability where
    traitKey x = AbilityKey $ abilityName x
    toTrait = AbilityTrait
    getTrait (AbilityTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Art where
    traitKey x = ArtKey $ take 2 $ artName x
    toTrait = ArtTrait
    getTrait (ArtTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Spell where
    traitKey x = SpellKey (spellFoTe x) (spellLevel x) (spellName x ) 
    toTrait = SpellTrait
    getTrait (SpellTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass VF where
    traitKey x = VFKey (vfname x) (vfDetail x)
    toTrait = VFTrait
    getTrait (VFTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass PTrait where
    traitKey x = PTraitKey $ ptraitName x
    toTrait = PTraitTrait
    getTrait (PTraitTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Reputation where
    traitKey x = ReputationKey ( reputationName x ) ( repLocale x )
    toTrait = ReputationTrait
    getTrait (ReputationTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Characteristic where
    traitKey x = CharacteristicKey ( characteristicName x ) 
    toTrait = CharacteristicTrait
    getTrait (CharacteristicTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Confidence where
    traitKey p = ConfidenceKey $ cname p
    toTrait = ConfidenceTrait
    getTrait (ConfidenceTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass OtherTrait where
    traitKey x = OtherTraitKey ( trait x ) 
    toTrait = OtherTraitTrait
    getTrait (OtherTraitTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass SpecialTrait where
    traitKey x = SpecialKey ( specialTrait x ) 
    toTrait = SpecialTraitTrait
    getTrait (SpecialTraitTrait x) = Just x
    getTrait _ = Nothing

instance TraitClass Age where
    traitKey _ = AgeKey
    toTrait = AgeTrait
    getTrait (AgeTrait x) = Just x
    getTrait _ = Nothing

instance TraitClass Possession where
    traitKey x = PossessionKey $ name x
    getTrait (PossessionTrait x) = Just x
    getTrait _ = Nothing
    toTrait = PossessionTrait

instance TraitClass CombatOption where
    traitKey x = CombatKey $ combatName x
    getTrait (CombatOptionTrait x) = Just x
    getTrait _ = Nothing
    toTrait = CombatOptionTrait

-- |
-- == Sorting 

(<:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(<:) p1 p2 = traitKey p1 < traitKey p2

{-
(>:) :: (TraitClass t1, TraitClass t2) => t1 -> t2 -> Bool
(>:) p1 p2 = p2 <: p1
-}


sortTraits :: TraitClass t => [ t ] -> [ t ]
sortTraits = sortBy f
       where f x y = compare (traitKey x) (traitKey y)

-- |
-- = Class instances

instance FromJSON Ability
instance FromJSON Characteristic 
instance FromJSON Art 
instance FromJSON Spell 
instance FromJSON PTrait 
instance FromJSON Reputation 
instance FromJSON VF 
instance FromJSON Confidence 
instance FromJSON OtherTrait 
instance FromJSON Trait  
instance ToJSON Ability
instance ToJSON Characteristic 
instance ToJSON Art 
instance ToJSON Spell 
instance ToJSON PTrait 
instance ToJSON Reputation 
instance ToJSON VF 
instance ToJSON Confidence 
instance ToJSON OtherTrait 
instance ToJSON Trait 
