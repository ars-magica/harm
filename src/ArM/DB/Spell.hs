{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.DB.Spell
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Spell Records
--
--
-----------------------------------------------------------------------------
module ArM.DB.Spell ( SpellRecord(..)
                       , spellDB
                       , spellLookup
                       , SpellDB
                       , isRitual
                       ) where

import ArM.DB.CSV
import ArM.Types
import GHC.Generics
import Data.List.Split
import Text.Read
import qualified Data.Map as M
import Data.List
import Data.Aeson
import Data.Aeson.Extra
-- import Data.Maybe

-- import ArM.Debug.Trace

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
    , rdt :: (String,String,String)   -- ^ Range/Duration/Target
    , specialSpell :: [String]        -- ^ Special tags, like Ritual or Mutantum.
    , spellDescription :: String           -- ^ Freeform description of the effect
    , design :: String                -- ^ Level calculation
    , spellComment :: String          -- ^ Freeform remarks that do not fit elsewhere
    , cite :: String                  -- ^ Source reference
    } deriving (Ord, Eq, Generic, Show)

instance ToJSON SpellRecord
instance FromJSON SpellRecord where
    parseJSON = withObject "SpellRecord" $ \v -> SpellRecord
        <$> v .: "name"
        <*> v .: "TeFo"
        <*> v .:? "level"
        <*> v .: "technique"
        <*> v `parseCollapsedList` "techniqueReq" 
        <*> v .: "form"
        <*> v `parseCollapsedList` "formReq" 
        <*> v .:? "rdt" .!= ("","","")
        <*> v .:? "specialSpell" .!= []
        <*> v .:? "description" .!= ""
        <*> v .:? "design" .!= ""
        <*> v .:? "comment" .!= []
        <*> v .:? "cite" .!= ""

-- | Default SpellRecord object as a starting point for step-by-step construction.
type SpellDB = M.Map String SpellRecord

-- | Create a `Data.Map.Map` of SpellRecord objects.  
-- The input is the output from `Data.CSV.csvFile`
spellDB :: [[String]] -> SpellDB
spellDB = M.fromList . map ( \ x -> (spellRecordName x,x) ) . map fromCSVline

instance ArMCSV SpellRecord where
   fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:_) =
      defaultObject { spellRecordName = x1 
                , spellRecordTeFo = x2
                , lvl = readMaybe x7
                , technique = x3
                , techniqueReq = filter (/="") $ splitOn ";" x4
                , form = x5
                , formReq = filter (/="") $ splitOn ";" x6
                , rdt = (x8, x9, x10)
                , specialSpell =  filter (/="") $ splitOn ";" x11
                , spellDescription = x12
                , design = x13
                , spellComment = x14
                , cite = x15
                }
   fromCSVline _ = defaultObject
   defaultObject = SpellRecord
                   { spellRecordName = ""
                   , spellRecordTeFo = ""
                   , lvl = Nothing
                   , technique = ""
                   , techniqueReq = []
                   , form = ""
                   , formReq = []
                   , rdt = ("Per","Mom","Ind")
                   , specialSpell = []
                   , spellDescription = ""
                   , design = ""
                   , spellComment = ""
                   , cite = ""
                   }
   getID = spellRecordName
spellLookup :: TraitKey -> SpellDB -> Maybe SpellRecord
spellLookup = M.lookup . spellKeyName
   


-- | Is the spell a ritual or not?
isRitual :: SpellRecord -> Bool
isRitual = f . find tf . specialSpell
    where tf = (=="Ritual") 
          f Nothing = False
          f (Just _) = True

