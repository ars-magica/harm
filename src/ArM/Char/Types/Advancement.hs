{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Types.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Advancement types representing changes over a season.
--
-----------------------------------------------------------------------------
module ArM.Char.Types.Advancement where

import ArM.Helper
import ArM.Char.Trait
import ArM.GameRules

import Data.Aeson 
-- import Data.Aeson.Types                          ( Parser )
import Control.Monad

import GHC.Generics
import Data.List.Split
import Text.Read             (readMaybe)

import Data.Text.Lazy                            ( fromStrict, unpack )

-- import Control.Applicative                       ( empty, pure, (<$>), (<*>) )

-- type CharTime = Maybe String

-- |
-- = Calendar
--
-- The software assumes the Hibernian calendar, with Winter being the first season of the year.
-- Several things may have to change for the standard calendar with Winter as the last season
-- of the year.  We have tried to collect these definitions here.

-- |
-- == Hibernian Calendar

-- | This comparison is used to check if a character is past the age limit where
-- aging rolls are required.
-- ```
-- (*>) = (>=)
-- ```
(>*) :: Ord a => a -> a -> Bool
(>*) = (>)

-- | Season of the year.
-- ```
-- data Season = Spring | Summer | Autumn | Winter | NoSeason
-- ```
data Season = Winter | Spring | Summer | Autumn  | NoSeason
     deriving (Show,Ord,Eq,Read,Generic)


-- |
-- == Generic definitions 

data SeasonTime = SeasonTime Season Int | GameStart | NoTime deriving (Eq,Generic)

instance ToJSON SeasonTime where
   toJSON = toJSON . show
-- instance FromJSON SeasonTime 
instance ToJSON Season



instance FromJSON SeasonTime where

    parseJSON (Number n) = pure $ SeasonTime NoSeason $ round n
    parseJSON (String t) = pure $ parseST (unpack (fromStrict t))
    parseJSON _ = mzero

isWinter :: SeasonTime -> Bool
isWinter (SeasonTime Winter _) = True
isWinter _ = False

parseST :: String -> SeasonTime
parseST  "GameStart" = GameStart
parseST  "Game Start" = GameStart
parseST  "Start" = GameStart
parseST  "Notime" = NoTime
parseST  "NoTime" = NoTime
parseST  "N/A" = NoTime
parseST  s = fy ys
    where xs = splitOn " " s
          ys = map readMaybe xs :: [Maybe Int]
          ss = map readMaybe xs :: [Maybe Season]
          fs [] = NoSeason
          fs (Nothing:rest) = fs rest
          fs (Just r:_) = r
          st = fs ss
          fy [] = NoTime
          fy (Nothing:rest) = fy rest
          fy (Just r:_) = SeasonTime st r
parseSeasonTime :: Maybe String -> SeasonTime
parseSeasonTime Nothing = NoTime
parseSeasonTime (Just s) = parseST s

instance Show SeasonTime where
   show GameStart = "Game Start"
   show (SeasonTime s y) = show s ++ " " ++ show y
   show NoTime =  "N/A"

instance Ord SeasonTime where
    (<=) NoTime _ = False
    (<=) _ NoTime = True
    (<=) GameStart _ = True
    (<=) _ GameStart = False
    (<=) (SeasonTime s1 y1) (SeasonTime s2 y2) 
        | y1 == y2 = s1 <= s2
        | otherwise = y1 <= y2




data AdvancementType = Practice | Exposure | Adventure 
                     | Teaching | Training | Reading | VisStudy
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teach | Train 
                  | Writing | Copying | OtherExposure | NoExposure
   deriving (Show,Ord,Eq)

data Resource = Resource String
   deriving (Eq,Show,Ord,Generic)
instance ToJSON Resource
instance FromJSON Resource

class AdvancementLike a where
     mode :: a -> Maybe String  -- ^ mode of study
     season :: a -> SeasonTime    -- ^ season or development stage
     narrative :: a -> Maybe String -- ^ freeform description of the activities
     uses :: a -> Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     sourceQuality :: a -> Maybe XPType -- ^ Source Quality (SQ)
     -- effectiveSQ :: Maybe Int   -- ^ SQ modified by virtues and flaws
     changes :: a -> [ ProtoTrait ]  -- ^ trait changes defined by player
     -- inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
data Advancement = Advancement 
     { advMode :: Maybe String  -- ^ mode of study
     , advSeason :: SeasonTime    -- ^ season or development stage
     , advYears :: Maybe Int    -- ^ number of years advanced
     , advNarrative :: Maybe String -- ^ freeform description of the activities
     , advUses :: Maybe [ Resource ] -- ^ Books and other resources used exclusively by the character
     , advSQ :: Maybe XPType -- ^ Source Quality (SQ) This should be the common SQ for adventures; individual variation should be recorded as `advBonus`
     , advBonus :: Maybe XPType -- ^ Bonus to Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     }
   deriving (Eq,Generic,Show)

defaultAdv :: Advancement 
defaultAdv = Advancement 
     { advMode = Nothing
     , advSeason = NoTime
     , advYears = Nothing
     , advNarrative = Nothing
     , advUses = Nothing
     , advSQ = Nothing
     , advBonus = Nothing
     , advChanges = [ ]  
     }

data Validation = ValidationError String | Validated String
   deriving (Eq,Generic)

instance Show Validation where
    show (ValidationError x) = "ERROR: " ++ x
    show (Validated x) = "Validated: " ++ x

-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { advancement :: Advancement -- ^ Base advancement as entered by the user
     , effectiveSQ :: Maybe XPType   -- ^ SQ modified by virtues and flaws
     , levelLimit :: Maybe Int   -- ^ spell level allowance
     , spentXP  :: Maybe XPType   -- ^ Total XP spent on advancement
     , inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws
     , augYears :: Maybe Int    -- ^ number of years advanced
     , validation :: [Validation] -- ^ Report from validation
     }
   deriving (Eq,Generic,Show)


defaultAA :: AugmentedAdvancement
defaultAA = Adv
     { advancement = defaultAdv
     , effectiveSQ = Nothing
     , levelLimit = Nothing 
     , spentXP = Nothing
     , inferredTraits = [ ] 
     , augYears = Nothing
     , validation = []
     }

instance AdvancementLike Advancement where
     mode = advMode
     season  = advSeason
     narrative  = advNarrative
     uses  = advUses
     sourceQuality  = advSQ
     changes = advChanges
instance AdvancementLike AugmentedAdvancement where
     mode a = advMode  $ advancement a
     season  = advSeason  .  advancement 
     narrative  a = advNarrative  $ advancement  a
     uses  a = advUses  $ advancement a
     sourceQuality  a =  advSQ  $ advancement a
     changes  a = advChanges  $ advancement  a

instance ToJSON Validation
instance FromJSON Validation

instance ToJSON AugmentedAdvancement where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Advancement where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode"
        -- <*> v .:? "season"
        <*> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "years"
        <*> v .:? "narrative"
        <*> v .:? "uses"
        <*> v .:? "sourceQuality"
        <*> v .:? "bonusQuality"
        <*> fmap maybeList ( v .:? "changes" )
instance FromJSON AugmentedAdvancement where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .: "advancement"
        <*> v .:? "effectiveSQ"
        <*> v .:? "levels"
        <*> v .:? "spentXP"
        <*> fmap maybeList ( v .:? "inferredTraits" )
        <*> v .:? "augYears"
        <*> fmap maybeList ( v .:?  "validation")

