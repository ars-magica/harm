{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Types.Trait
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
module ArM.Char.Types.Trait where

import ArM.GameRules
import ArM.Helper
import ArM.Char.Types.TraitKey
import ArM.DB.Weapon

import GHC.Generics
import Data.Text.Lazy                            ( fromStrict, unpack )
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.List (sortBy)
import Control.Monad

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
           | PossessionTrait { item :: Possession, count :: Int }
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
                                     , agingPoints :: Int }
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

-- | Return a string of Technique/Form/Level classifying the Spell.
spellTeFoLe :: Spell -> String
spellTeFoLe sp = spellTeFo sp ++ show (spellLevel sp)

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

data Age = Age
    { ageYears :: Int             -- ^ character age in years
    , apparentYounger :: Int      -- ^ difference between age and apparent age
    , ageLimit :: Int
    , longevityRitual :: Int      -- ^ Score of longevity ritual (LR), negative number means none
    , agingRollBonus :: Int       -- ^ Bonus to aging rolls (excluding LR)
    , ageComment :: Maybe String  -- ^ freeform comment
    } deriving (Show,Ord,Eq,Generic)
instance ToJSON Age
instance FromJSON Age 

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
instance Show Reputation  where
   show a = reputationName a ++ " [" ++ (repLocale a) ++ "] "
          ++ show (repScore a) ++ " (" ++ showNum (repExcessXP a) ++ ") "


-- |
-- == Weapons and other Possessions


data Vis = Vis
    { visArt :: String
    , visDescription :: Maybe String
    } deriving ( Show, Ord, Eq, Generic )

data Possession = WeaponPossession Weapon 
                | Possession String 
                | VisPossession Vis 
                | ArmourPossession Armour 
                | StandardPossession StandardItem
    deriving ( Show, Ord, Eq, Generic )
data StandardItem = StandardItem { weapon :: Maybe String, armour :: Maybe String }
    deriving ( Show, Ord, Eq, Generic )
instance FromJSON Possession where
    parseJSON (String t) = pure $ Possession (unpack (fromStrict t))
    parseJSON (Object v) | isJust w = pure $ WeaponPossession $ fromJust w
                         | isJust ar = pure $ ArmourPossession $ fromJust ar
                         | isJust std = pure $ StandardPossession $ fromJust std
                         | isJust vis = pure $ VisPossession $ fromJust vis
                         | otherwise = mzero
       where w = parseAny ob
             ar = parseAny ob
             std = parseAny ob
             vis = parseAny ob
             ob = Object v
    parseJSON _ = mzero

-- | Parse JSON or return Nothing.
-- This is a helper for the FromJSON instance of Possession.
parseAny :: FromJSON a => Value -> Maybe a
parseAny = parseMaybe parseJSON

instance ToJSON Possession where
    toJSON (WeaponPossession x) = toJSON x
    toJSON (ArmourPossession x) = toJSON x
    toJSON (StandardPossession x) = toJSON x
    toJSON (Possession x) = toJSON x
    toJSON (VisPossession x) = toJSON x
instance FromJSON Vis 
instance ToJSON Vis 
instance FromJSON StandardItem 
instance ToJSON StandardItem 

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
    traitKey (PossessionTrait x _) = traitKey x
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
    traitKey (WeaponPossession x) = PossessionKey (weaponName x)
    traitKey (ArmourPossession x) = PossessionKey (armourName x)
    traitKey (StandardPossession x) = PossessionKey y
        where y | isJust (weapon x) = "Weapon: " ++ (fromJust $ weapon x)
                | isJust (armour x) = "Armour: " ++ (fromJust $ armour x)
                | otherwise         = "Nothing"
    traitKey (Possession x) = PossessionKey x
    traitKey (VisPossession x) = PossessionKey $ show x
    toTrait x = PossessionTrait x 1
    getTrait (PossessionTrait x _) = Just x
    getTrait _ = Nothing


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
