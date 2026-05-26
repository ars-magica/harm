{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait.Possession
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Possession ranging weapons and books to enchanted devices
--
-- Possessions are physical objects which can be gained, lost, given away,
-- or traded.  
--
-- Possessions can be held by either a covenant or a character.  For characters
-- they are embedded in a special kind of trait.
--
-----------------------------------Types.------------------------------------------
module ArM.Trait.Possession ( -- * Posessions
                              Possession(..)
                            , defaultPossession
                            -- * Magic and Enchantments
                            , Enchantment(..)
                            , MagicEffect(..)
                            , isMagic
                            , isVis
                            , isVisSrc
                            , isAC
                            , isStaff
                            , effectRDT
                            -- * Weapons and Mundane Equipment
                            , isComposite
                            , isMundaneEquipment
                            , isWeapon
                            , isArmour
                            , isEquipment
                            -- * Books
                            , isBook
                            , isLabText
                            , LabText(..)
                            , wrapBooks
                            , textLevel
                            -- * Unused -- to be reviewed
                            , toPossession  
                            , isTractatus
                            , parseCharged
                            , parseTalisman
                            , parseGreater
                            , parseLesser
                            , enchantmentName
                            ) where

import ArM.Story
import ArM.Trait.Trait
import ArM.Trait.SpellRecord
import ArM.Helper

-- import GHC.Generics
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Extra
import Data.Aeson.Types
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad
import Control.Applicative ((<|>))
import Data.Maybe

-- | A 'RawPossession' distinguishes between different kinds of possession,
-- representing easier syntax patterns for the YAML files.
-- It is was introduced to simplify parsing, but is not currently used.
data RawPossession = CompositeItem Possession 
                   | SimpleBook Book
                   | SimpleItem String
    deriving (Show)


instance ToJSON RawPossession where
    toJSON (CompositeItem ob) = object [(fromString "item",toJSON ob)]
    toJSON (SimpleBook ob) = object [(fromString "book",toJSON ob)]
    toJSON (SimpleItem ob) = toJSON ob

instance FromJSON RawPossession where
    parseJSON (String t) = pure $ SimpleItem (unpack (fromStrict t)) 
    parseJSON (Object v) = (CompositeItem <$> v .: "item") 
                         <|> (SimpleBook <$> v .: "book")
    parseJSON _ = mzero

toPossession :: RawPossession -> Possession
toPossession (CompositeItem ob) = ob
toPossession (SimpleBook ob) = wrapBook ob
toPossession (SimpleItem st) = setName st defaultPossession

enchantmentName :: Enchantment -> String
enchantmentName (LesserItem e) = effectName e
enchantmentName (ChargedItem _ e) = effectName e
enchantmentName (GreaterDevice _ (e:_)) = effectName e
enchantmentName (Talisman _ _) = "Talisman"
enchantmentName _ = ""

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

isVis :: Possession -> Bool
isVis = (/=0) . pawns 

isVisSrc :: Possession -> Bool
isVisSrc = (/=0) . visYield 

isWeapon :: Possession -> Bool
isWeapon p = (weapon p /= []) || (weaponStats p /= [])

isArmour :: Possession -> Bool
isArmour p = (armour p /= []) || (armourStats p /= [])

isMagic :: Possession -> Bool
isMagic p = enchantment p /= MundaneItem

isAC :: Possession -> Bool
isAC p = isJust $ acTo p

isStaff :: Possession -> Bool
isStaff = isJust . staff 

isMundaneEquipment :: Possession -> Bool
isMundaneEquipment p = isEquipment p && (not . isMagic) p && (
     silver p == 0 && silverYield p == 0 )

isEquipment :: Possession -> Bool
isEquipment p = not $ foldl (||) False [ f p | f <- fs ] 
   where fs = [ isVis, isVisSrc, isWeapon, isArmour, isAC, isBook, isLabText, isStaff ]

-- ! Is the item a composite item with traits of more than one kind,
-- such as an enchanted book, or a weapon that is an arcane connection?
--
-- Items which are not composite should usually be displayed in a compact 
-- format, reflecting its kind.  Composite items may require more verbose
-- presentation.
isComposite :: Possession -> Bool
isComposite x = ln fs
   where ln = (1<) . length . filter (==True) . map ($ x)
         fs =  [ isVis, isWeapon, isArmour, isAC, isBook, isLabText, isMagic ]

-- == Books

-- | The level of a lab text, derived from the effect or spell it describes.
textLevel :: LabText -> Int
textLevel (Device ob) = effectLevel ob
textLevel (SpellText ob) = fromMaybe 0 $ lvl ob


effectRDT :: MagicEffect -> String
effectRDT eff = showStrList [ r, d, t ]
   where r = f "Range" (effectRange eff)
         d = f "Duration" (effectDuration eff)
         t = f "Target" (effectTarget eff)
         f _ "" = ""
         f s x = s ++ ": " ++ x

-- | Is the book a tractatus or something else?
isTractatus :: Book -> Bool
isTractatus = f . bookStats 
    where f [] = False
          f (x:_) = isJust ( quality x ) && isNothing ( bookLevel x )

-- | Is the item a book?
isBook :: Possession -> Bool
isBook p = bookTexts p /= []

-- | Does the item contain lab texts?
isLabText :: Possession -> Bool
isLabText p = labTexts p /= []

-- | Wrap a book as a possesion
wrapBook :: Book -> Possession
wrapBook b = defaultPossession 
             { bookTexts = [ b ]
             , itemCount = bookCount b
             }

-- | Wrap a list of books as possesions
wrapBooks :: [Book] -> [Possession]
wrapBooks = map wrapBook
