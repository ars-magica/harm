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
import ArM.Types.Calendar
import ArM.GameRules

import Data.Char 
import Data.Aeson 
import GHC.Generics
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad


-- |
-- = Advancement

data AdvancementType = Practice  | Adventure | Taught
                     | Trained | Reading | VisStudy
                     | Exposure ExposureType
                     | CharGen String
   deriving (Show,Ord,Eq)
data ExposureType = LabWork | Teaching | Training
                  | Writing | Copying | Authoring
                  | Initiation | OpeningArts | Work
                  | OtherExposure String
   deriving (Show,Ord,Eq)

instance ToJSON AdvancementType where
   toJSON = toJSON . show
instance FromJSON AdvancementType where

    parseJSON (String t) = pure $ parseAT (unpack (fromStrict t))
    parseJSON _ = mzero


parseET :: String -> ExposureType
parseET = f . map toLower . trim
  where f x | take 3 x == "lab" = LabWork
            | take 3 x == "tea" = Teaching
            | take 3 x == "tra" = Training
            | take 3 x == "wri" = Authoring
            | take 3 x == "aui" = Authoring
            | take 3 x == "cop" = Copying
            | take 3 x == "ini" = Initiation
            | take 3 x == "ope" = OpeningArts
            | take 3 x == "wor" = Work
            | take 3 x == "oth" = OtherExposure $ dropWord $ trim x
            | otherwise = OtherExposure x
parseAT :: String -> AdvancementType
parseAT = f . map toLower . trim
  where f x | take 3 x == "pra" = Practice
            | take 3 x == "adv" = Adventure
            | take 3 x == "tau" = Taught
            | take 3 x == "tea" = Taught
            | take 3 x == "tra" = Trained
            | take 3 x == "rea" = Reading
            | take 3 x == "vis" = VisStudy
            | take 3 x == "cha" = CharGen $ dropWord $ trim x
            | take 3 x == "exp" = Exposure $ parseET $ dropWord $ trim x
            | g (ex x) = Exposure (ex x)
            | otherwise = CharGen x 
        ex x = parseET x
        g (OtherExposure _) = False
        g _ = True
dropWord :: String -> String
dropWord "" = ""
dropWord (x:xs) | isSpace x = trim xs
                | otherwise = dropWord xs


{-
data Resource = Resource String
   deriving (Eq,Show,Ord,Generic)
instance ToJSON Resource
instance FromJSON Resource
-}

class AdvancementLike a where
     mode :: a -> AdvancementType  -- ^ mode of study
     season :: a -> SeasonTime    -- ^ season or development stage
     narrative :: a -> Maybe String -- ^ freeform description of the activities
     usesBook :: a -> [ String ] -- ^ Books used exclusively by the character
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
     { advMode :: AdvancementType -- ^ mode of study
     , advSeason :: SeasonTime    -- ^ season or development stage
     , advYears :: Maybe Int    -- ^ number of years advanced
     , advNarrative :: Maybe String -- ^ freeform description of the activities
     , advUses :: [ String ] -- ^ Books used exclusively by the character
     , advSQ :: Maybe XPType -- ^ Source Quality (SQ) This should be the common SQ for adventures; individual variation should be recorded as `advBonus`
     , advBonus :: Maybe XPType -- ^ Bonus to Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     }
   deriving (Eq,Generic,Show)

defaultAdv :: Advancement 
defaultAdv = Advancement 
     { advMode = CharGen "Nothing"
     , advSeason = NoTime
     , advYears = Nothing
     , advNarrative = Nothing
     , advUses = []
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
     , postProcessTrait :: PostProcessor 
        -- ^ extra postprocessing for traits at a given stage 
     }
   deriving (Eq,Show,Generic)
data PostProcessor = PostProcessor (Trait -> Trait)

instance Eq PostProcessor where
   (==) _ _ = True
instance Show PostProcessor where
   show _ = ""
instance FromJSON PostProcessor where
   parseJSON _ = return $ PostProcessor id
instance ToJSON PostProcessor where
   toJSON _ = "{}"


defaultAA :: AugmentedAdvancement
defaultAA = Adv
     { advancement = defaultAdv
     , effectiveSQ = Nothing
     , levelLimit = Nothing 
     , spentXP = Nothing
     , inferredTraits = [ ] 
     , augYears = Nothing
     , validation = []
     , postProcessTrait = PostProcessor id
     }

instance AdvancementLike Advancement where
     mode = advMode
     season  = advSeason
     narrative  = advNarrative
     usesBook = advUses
     sourceQuality  = advSQ
     changes = advChanges
instance AdvancementLike AugmentedAdvancement where
     mode a = advMode  $ advancement a
     season  = advSeason  .  advancement 
     narrative  a = advNarrative  $ advancement  a
     usesBook  a = advUses  $ advancement a
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
        <$> v .:? "mode" .!= CharGen "Nothing"
        -- <*> v .:? "season"
        <*> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "years"
        <*> v .:? "narrative"
        <*> v .:? "usesBook"    .!= []
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
        <*> v .:? "postProcessTrait" .!= PostProcessor id

