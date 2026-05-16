{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Trait.SpellRecord
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Spell Records
--
--
-----------------------------------------------------------------------------
module ArM.Trait.SpellRecord ( SpellRecord(..)
                       , defaultSpellRecord
                       , isRitual
                       , rdt
                       ) where

import GHC.Generics
import Data.List
import Data.Aeson
import Data.Aeson.Extra

-- | A SpellRecord is a shared object describing a spell.
-- It is different from the Spell trait which represents the individual's
-- knowledge of the spell.
-- Note that the SpellRecord is identified by spell name, so that generic
-- level spells share a record.  The SpellKey is used to sort traits and
-- includes the level of the instance.
data SpellRecord = SpellRecord
    { spellRecordName :: String -- ^ Name of the spell
    , spellRecordTeFo :: String -- ^ Technique/Form abreviation for the spell
    , lvl :: Maybe Int       -- ^ Spell Level.  General Level Spells have Nothing.
    , technique :: String
    , techniqueReq :: [String]
    , form :: String
    , formReq :: [String]
    , spellRange :: String
    , spellDuration :: String
    , spellTarget :: String
    , specialSpell :: [String]        -- ^ Special tags, like Ritual or Mutantum.
    , spellDescription :: String           -- ^ Freeform description of the effect
    , design :: String                -- ^ Level calculation
    , spellComment :: String          -- ^ Freeform remarks that do not fit elsewhere
    , cite :: String                  -- ^ Source reference
    } deriving (Ord, Eq, Generic, Show)
defaultSpellRecord :: SpellRecord
defaultSpellRecord = SpellRecord
    { spellRecordName = ""
    , spellRecordTeFo = ""
    , lvl = Nothing
    , technique = ""
    , techniqueReq = []
    , form = ""
    , formReq = []
    , spellRange = "Personal"
    , spellDuration = "Momentary"
    , spellTarget = "Ind"
    , specialSpell = []
    , spellDescription = ""
    , design = ""
    , spellComment = ""
    , cite = ""
    } 

-- | The range/duration/target of the given spell
rdt :: SpellRecord -> (String,String,String)   -- ^ Range/Duration/Target
rdt x = (spellRange x,spellDuration x,spellTarget x)

instance ToJSON SpellRecord
instance FromJSON SpellRecord where
    parseJSON = withObject "SpellRecord" $ \v -> SpellRecord
        <$> v .: "name"
        <*> v .: "TeFo"
        <*> v .:? "level"
        <*> fmap (take 2) (v .: "technique")
        <*> v `parseCollapsedList` "techniqueReq" 
        <*> fmap (take 2) (v .: "form")
        <*> v `parseCollapsedList` "formReq" 
        <*> v .:? "range" .!= ""
        <*> v .:? "duration" .!= ""
        <*> v .:? "target" .!= ""
        <*> v .:? "specialSpell" .!= []
        <*> v .:? "description" .!= ""
        <*> v .:? "design" .!= ""
        <*> v .:? "comment" .!= []
        <*> v .:? "cite" .!= ""


-- | Is the spell a ritual or not?
isRitual :: SpellRecord -> Bool
isRitual = f . find tf . specialSpell
    where tf = (=="Ritual") 
          f Nothing = False
          f (Just _) = True

