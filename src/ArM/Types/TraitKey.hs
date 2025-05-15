{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.TraitKey
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  TraitKey type used to index and sort traits.
--
-- We use `TraitKey` objects to reference arbitrary traits in a variety
-- of contexts.  It is conceptually simple, but the ordering takes some
-- coding to handle all the different kinds of traits.  To avoid
-- cluttering, it is kept as a separate module.
--
-- JSON support is provided, where most traits are represented as an
-- object of the form { kind : trait } or `{ "art" : "Creo" }`.
-- Only a few traits require additional information to disambiguate.
--
-----------------------------------------------------------------------------
module ArM.Types.TraitKey ( TraitKey(..), isVF, artKey ) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

artKey :: String -> TraitKey
artKey = ArtKey . take 2

-- | A unique identifier for traits.
-- It implement the `Ord` class, with canonical ordering on characteristics
-- and arts.
data TraitKey = AbilityKey String
           | CharacteristicKey String
           | ArtKey String
           | SpellKey String Int String 
           | PTraitKey String
           | ReputationKey String String
           | VFKey String String
           | ConfidenceKey String
           | OtherTraitKey String
           | PossessionKey String
           | EstateKey String
           | CombatKey String
           | AgeKey
           | NoTrait
           deriving ( Eq, Generic )

instance Show TraitKey where
           show (CharacteristicKey x) = x
           show (AbilityKey x) = x
           show (ArtKey x) = x
           show (SpellKey x y z) = z ++ show y ++ " " ++ x
           show (PTraitKey x) = "Personality Trait: " ++ x
           show (ReputationKey x y) = x ++ " [" ++ y ++ "]"
           show (VFKey x y) = x ++ " [" ++ y ++ "]"
           show (ConfidenceKey x) = x
           show (OtherTraitKey x) = x
           show (PossessionKey x) = "Possession: " ++ x
           show (EstateKey x) = "Estate: " ++ x
           show (CombatKey x) = "Combat Option: " ++ x
           show AgeKey = "Age Trait"
           show NoTrait = "No Trait"
instance Ord TraitKey where
   compare (AbilityKey x) (AbilityKey y) = compare x y
   compare (CharacteristicKey x) (CharacteristicKey y) = compare (charIdx x) (charIdx y)
   compare (ArtKey x) (ArtKey y) = compare (artIdx x) (artIdx y)
   compare (SpellKey x1 x2 x3) (SpellKey y1 y2 y3) = compare (x1,x2,x3) (y1,y2,y3)
   compare (PTraitKey x) (PTraitKey y) = compare x y
   compare (ReputationKey x1 x2) (ReputationKey y1 y2) = compare (x1,x2) (y1,y2)
   compare (VFKey x1 x2) (VFKey y1 y2) = compare (x1,x2) (y1,y2)
   compare (ConfidenceKey x) (ConfidenceKey y) = compare x y
   compare (OtherTraitKey x) (OtherTraitKey y) = compare x y
   compare (PossessionKey x) (PossessionKey y) = compare x y
   compare (EstateKey x) (EstateKey y) = compare x y
   compare (CombatKey x) (CombatKey y) = compare x y
   compare AgeKey AgeKey = EQ
   compare NoTrait NoTrait = EQ
   compare (AbilityKey _) _ = LT
   compare _ (AbilityKey _) = GT
   compare (CharacteristicKey _) _ = LT
   compare _ (CharacteristicKey _) = GT
   compare (ArtKey _) _ = LT
   compare _ (ArtKey _) = GT
   compare (SpellKey _ _ _) _ = LT
   compare _ (SpellKey _ _ _) = GT
   compare (PTraitKey _) _ = LT
   compare _ (PTraitKey _) = GT
   compare (ReputationKey _ _) _ = LT
   compare _ (ReputationKey _ _) = GT
   compare (VFKey _ _) _ = LT
   compare _ (VFKey _ _) = GT
   compare (ConfidenceKey _) _ = LT
   compare _ (ConfidenceKey _) = GT
   compare (OtherTraitKey _) _ = LT
   compare _ (OtherTraitKey _) = GT
   compare (PossessionKey _) _ = LT
   compare _ (PossessionKey _) = GT
   compare (EstateKey _) _ = LT
   compare _ (EstateKey _) = GT
   compare (CombatKey _) _ = LT
   compare _ (CombatKey _) = GT
   compare AgeKey _ = LT
   compare _ AgeKey = GT
   -- compare AgeKey _ = GT
   -- compare _ AgeKey = LT

-- | List of arts defined in *Ars Magica*
artIdx :: String -> Int
artIdx "Cr" = 1
artIdx "In" = 2
artIdx "Mu" = 3
artIdx "Pe" = 4
artIdx "Re" = 5
artIdx "An" = 11
artIdx "Aq" = 12
artIdx "Au" = 13
artIdx "Co" = 14
artIdx "He" = 15
artIdx "Ig" = 16
artIdx "Im" = 17
artIdx "Me" = 18
artIdx "Te" = 19
artIdx "Vi" = 20
artIdx _    = 100

charIdx :: String -> Int
charIdx "Int" = 1
charIdx "Per" = 2
charIdx "Pre" = 3
charIdx "Com" = 4
charIdx "Str" = 5
charIdx "Sta" = 6
charIdx "Dex" = 7
charIdx "Qik" = 8
charIdx _ = 9

instance ToJSON TraitKey
instance FromJSON TraitKey where
    parseJSON = fmap convertProtoKey . parseJSON

-- | Intermediate type used in the JSON parser for `TraitKey`.
data ProtoKey = ProtoKey
   { ability :: Maybe String
   , characteristic :: Maybe String
   , art :: Maybe String
   , spell :: Maybe String
   , spellLevel :: Maybe Int 
   , detail :: Maybe String 
   , personality :: Maybe String
   , reputation :: Maybe String 
   , vf :: Maybe String 
   , confidence :: Maybe String
   , other :: Maybe String
   , special :: Maybe String
   , possession :: Maybe String
   , lab :: Maybe String
   , combat :: Maybe String
   }
   deriving ( Eq, Generic )

instance FromJSON ProtoKey where
    parseJSON = withObject "ProtoKey" $ \v -> ProtoKey
        <$> v .:? "ability" 
        <*> v .:? "characteristic" 
        <*> v .:? "art" 
        <*> v .:? "spell" 
        <*> v .:? "spellLevel" 
        <*> v .:? "detail" 
        <*> v .:? "personality" 
        <*> v .:? "reputation" 
        <*> v .:? "vf" 
        <*> v .:? "confidence" 
        <*> v .:? "other" 
        <*> v .:? "special" 
        <*> v .:? "possession" 
        <*> v .:? "lab" 
        <*> v .:? "combat" 

-- | Auxiliary function for the JSON parser for `TraitKey`.
convertProtoKey :: ProtoKey -> TraitKey
convertProtoKey p 
  | isJust (ability p) = AbilityKey (fromJust $ ability p)
  | isJust (art p) = ArtKey (fromJust $ art p)
  | isJust (characteristic p) = CharacteristicKey (fromJust $ characteristic p)
  | isJust (spell p) = SpellKey (fromJust $ spell p) (fromMaybe (-1) $ spellLevel p) (fromMaybe "" $ detail p)
  | isJust (personality p) = PTraitKey (fromJust $ personality p) 
  | isJust (reputation p) = ReputationKey (fromJust $ reputation p) (fromMaybe "" $ detail p)
  | isJust (vf p) = VFKey (fromJust $ vf p) (fromMaybe "" $ detail p)
  | isJust (confidence p) = ConfidenceKey (fromJust $ confidence p) 
  | isJust (other p) = OtherTraitKey (fromJust $ other p) 
  | isJust (possession p) = PossessionKey (fromJust $ possession p) 
  | isJust (lab p) = EstateKey (fromJust $ lab p) 
  | isJust (combat p) = CombatKey (fromJust $ combat p) 
  | otherwise = NoTrait

isVF :: TraitKey -> Bool
isVF (VFKey _ _) = True
isVF  _  = False
