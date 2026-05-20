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
module ArM.Trait.SpellRecord where
   -- SpellRecord(..), MagicEffect(..), defaultSpellRecord, isRitual, rdt

import GHC.Generics
import ArM.Story
import ArM.Helper
import Data.List
import Data.Maybe
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


instance ToJSON SpellRecord
instance FromJSON SpellRecord where
    parseJSON = withObject "SpellRecord" $ \v -> SpellRecord
        <$> v .: "name"
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

instance StoryObject SpellRecord where
   name ob = spellRecordName ob 
   setName n x = x { spellRecordName = n }
   narrative ob = [ spellDescription ob ]
   addNarrative s x = x { spellDescription = prependString (narrative x) s }
   comment ob = [ spellComment ob ]
   addComment s x = x { spellComment = prependString (comment x) s }


instance StoryObject MagicEffect where
   name ob = effectName ob 
   setName n x = x { effectName = n }
   narrative ob = effectDescription ob
   addNarrative s x = x { effectDescription = s:narrative x }
   comment ob = effectComment ob
   addComment s x = x { effectComment = s:comment x }

class StoryObject a => SpellLike a where
   -- | Return the spell signature with name and TeFo/level, in markdown format.
   spellSignatureMD :: a -> String
   spellSignatureMD s = "*" ++ name s ++ "* (" ++ tefol s ++ ")"
   -- | Return the TeFo and Level in abbreviated format.
   tefol :: a -> String
   tefol s = tefo s ++ (fromMaybe "*" $ fmap show $ level s)
   -- | Return the TeFo Level with requisites.
   teforq :: a -> String
   teforq s = ( magicTechnique s ++ rt ++ magicForm s ++ rf 
              ++ show (level s) )
      where rt = f $ reqTechnique s
            rf = f $ reqForm s
            f = b . foldl (++) "" . map (take 2)
            b "" = ""
            b x = "(" ++ x ++ ")"
   -- | Return the TeFo and Level with requisites.
   teforql :: a -> String
   teforql s = teforq s ++ (fromMaybe "*" $ fmap show $ level s)
   -- | Return the TeFo in abbreviated format without requisistes.
   tefo :: a -> String
   tefo x = magicTechnique x ++ magicForm x
   -- | Return the level
   level :: a -> Maybe Int
   -- | The range/duration/target of the given spell
   rdt :: a -> (String,String,String)   -- ^ Range/Duration/Target
   magicTechnique :: a -> String
   magicForm :: a -> String
   reqTechnique :: a -> [ String ]
   reqForm:: a -> [ String ]


instance SpellLike SpellRecord where
   level = lvl
   rdt x = (spellRange x,spellDuration x,spellTarget x)
   magicTechnique = take 2 . technique
   magicForm = take 2 . form
   reqTechnique = techniqueReq
   reqForm = formReq

instance SpellLike MagicEffect where
   level = Just . effectLevel
   rdt x = (effectRange x,effectDuration x,effectTarget x)
   magicTechnique = take 2 . effectTechnique
   magicForm = take 2 . effectForm
   reqTechnique = effectTechniqueReq
   reqForm = effectFormReq

