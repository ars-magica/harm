{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Types.TraitKey
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  TraitKey type used to index and sort traits.
--
-----------------------------------------------------------------------------
module ArM.Char.Types.TraitKey where

import Data.Aeson
import GHC.Generics

data TraitKey = AbilityKey String
           | CharacteristicKey String
           | ArtKey String
           | SpellKey String Int String 
           | PTraitKey String
           | ReputationKey String String
           | VFKey String String
           | ConfidenceKey String
           | OtherTraitKey String
           | SpecialKey String
           | PossessionKey String
           | CombatKey String
           | AgeKey
           | NoTrait
           deriving (Show, Eq,Generic )
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
   compare (SpecialKey x) (SpecialKey y) = compare x y
   compare (PossessionKey x) (PossessionKey y) = compare x y
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
   compare (SpecialKey _) _ = LT
   compare _ (SpecialKey _) = GT
   compare (PossessionKey _) _ = LT
   compare _ (PossessionKey _) = GT
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
instance FromJSON TraitKey
