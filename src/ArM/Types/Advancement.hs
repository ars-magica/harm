{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Advancement types representing Character improvement.
--
-- Advancement is tricky because there are many different modes of study
-- and many special cases.  The basic structure uses two main types:
-- + Advancement is the changes defined by the user. 
-- + AugmentedAdvancement comprises the `Advancement` object and additional
--   inferred changes.
--
-- The Source Quality (SQ) is controled by several fields.
-- + AugmentedAdvancement `baseSQ` is the base source quality inferred
--   from other traits and general rules.
-- + Advancement `sourceQuality` allows the user to enter the basic
--   SQ manually.  This is required for Practice, and may be used to 
--   override the `baseSQ`.  If it differs from `baseSQ`, a warning
--   is issued.
-- + AugmentedAdvancement `bonusSQ` includes modifications derived
--   automatically from virtues and flaws, and other individual
--   circumstances.
-- + Advancement `bonusQuality` should be used for individual modifications
--   that cannot be automatically inferred, such as correspondent and Study
--   Bonus.
--
-- Also not the `scoreCap` or `sourceCap` that caps the score that can
-- be acquired from the source. This applies to books, trainers, and teachers.
--
-- The `Advancement` object has a list of `changes` which is `ProtoTrait`
-- objects modifying existing traits.  Similarly, `AugmentedAdvancement`
-- has `inferredTraits` for additional implied changes.
--
-----------------------------------------------------------------------------
module ArM.Types.Advancement where

import ArM.Helper
import ArM.Types.ProtoTrait
import ArM.Types
import ArM.GameRules
import ArM.Types.Library

import Data.Maybe 
import Data.List 
import Data.Char 
import Data.Aeson 
import Data.Aeson.Extra
import GHC.Generics
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad


-- |
-- = Advancement

-- |
-- == Advancement Types

-- |
-- Main advancement modes as defined in the core rules [ArM]
data AdvancementType = Practice  | Adventure | Taught
                     | Trained | Reading | VisStudy
                     | Exposure ExposureType
                     | CharGen String
   deriving (Ord,Eq)

-- |
-- Different activities that grant Exposure but may require different
-- processing in advancement and validation.
data ExposureType = LabWork | Teaching | Training
                  | Writing | Copying | Authoring
                  | Initiation | OpeningArts | Work
                  | SpellInstruction
                  | OtherExposure String
   deriving (Show,Ord,Eq)

instance Show AdvancementType where
   show Practice   = "Practice"
   show Adventure  = "Adventure"
   show Taught = "Taught"
   show Trained  = "Trained"
   show Reading  = "Reading"
   show VisStudy = "Vis Study"
   show (Exposure (OtherExposure x)) = show x ++ " (Other Exposure)"
   show (Exposure x) = show x ++ " (Exposure)"
   show (CharGen x) = x 


instance ToJSON AdvancementType where
   toJSON = toJSON . show
instance FromJSON AdvancementType where
    parseJSON (String t) = pure $ parseAT (unpack (fromStrict t))
    parseJSON _ = mzero

-- |
-- Parse an ExposureType from a String, auxiliary for `FromJSON`
parseET :: String -> ExposureType
parseET x' = f $  trim x
  where f y | take 3 y == "lab" = LabWork
            | take 3 y == "tea" = Teaching
            | take 3 y == "tra" = Training
            | take 3 y == "wri" = Authoring
            | take 3 y == "aut" = Authoring
            | take 3 y == "cop" = Copying
            | take 3 y == "ini" = Initiation
            | take 3 y == "ope" = OpeningArts
            | take 3 y == "spe" = SpellInstruction
            | take 3 y == "wor" = Work
            | take 3 y == "oth" = OtherExposure $ dropWord $ trim x'
            | otherwise = OtherExposure x'
        x = map toLower x'

-- |
-- Parse an AdvancementType from a String, auxiliary for `FromJSON`
parseAT :: String -> AdvancementType
parseAT x' = f $ trim x
  where f y | take 3 y == "pra" = Practice
            | take 3 y == "adv" = Adventure
            | take 3 y == "tau" = Taught
            | take 3 y == "tra" = Trained
            | take 3 y == "rea" = Reading
            | take 3 y == "vis" = VisStudy
            | take 7 y == "chargen" = CharGen $ dropWord $ trim x'
            | take 3 y == "exp" = Exposure $ parseET $ dropWord $ trim x'
            | g (ex y) = Exposure (ex x')
            | otherwise = CharGen x'
        ex y = parseET y
        g (OtherExposure _) = False
        g _ = True
        x = map toLower x'

-- |
-- Drop the first word from a string.  
dropWord :: String -> String
dropWord "" = ""
dropWord (x:xs) | isSpace x = trim xs
                | otherwise = dropWord xs


-- |
-- == AdvancementLike Class

-- |
-- The AdvancementLike class gives a common API to Advancement and
-- AugmentedAdvanceemnt
class StoryObject a => AdvancementLike a where
     mode :: a -> AdvancementType  -- ^ mode of study
     usesBook :: a -> [ String ] -- ^ Books used exclusively by the character
     sourceQuality :: a -> Maybe XPType -- ^ Source Quality (SQ)
     sourceCap :: a -> Maybe Int -- ^ Level cap from the source of learning
     changes :: a -> [ ProtoTrait ]  -- ^ trait changes defined by player
     isExposure :: a -> Bool
     isExposure = f . mode
        where f (Exposure _) = True
              f _ = False

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
-- Note that standard SQ should be recorded as `advSQ`, while individual
-- variation may be recorded as `advBonus`.
data Advancement = Advancement 
     { advMode :: AdvancementType -- ^ mode of study
     , advSeason :: SeasonTime    -- ^ season or development stage
     , advYears :: Maybe Int    -- ^ number of years advanced
     , advNarrative :: [ String ] -- ^ narrative description of the activities
     , advComment :: [ String ]   -- ^ freeform description of the activities
     , advUses :: [ String ] -- ^ Books used exclusively by the character
     -- , advReadTome :: String -- ^ Tome studied
     -- , advReadBook :: String -- ^ Partook studied
     -- , advReadTrait :: String -- ^ Book studied
     , advSQ :: Maybe XPType -- ^ Source Quality (SQ) 
     , advCap :: Maybe Int   -- ^ Source Quality (SQ) 
     , advBonus :: Maybe XPType -- ^ Bonus to Source Quality (SQ)
     , advChanges :: [ ProtoTrait ]  -- ^ trait changes defined by player
     }
   deriving (Eq,Generic,Show)

defaultAdv :: Advancement 
defaultAdv = Advancement 
     { advMode = CharGen "Nothing"
     , advSeason = NoTime
     , advYears = Nothing
     , advNarrative = []
     , advComment = []
     , advUses = []
     , advSQ = Nothing
     , advCap = Nothing
     , advBonus = Nothing
     , advChanges = [ ]  
     }

instance ToJSON Advancement where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Advancement where
    parseJSON = withObject "Advancement" $ \v -> Advancement
        <$> v .:? "mode" .!= CharGen "Nothing"
        <*> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "years"
        <*> v `parseCollapsedList` "narrative" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "usesBook"    .!= []
        <*> v .:? "sourceQuality"
        <*> v .:? "sourceCap"
        <*> v .:? "bonusQuality"
        <*> v .:? "changes" .!= []


-- |
-- == The Augmented Advancement

-- | Advancement with additional inferred fields
data AugmentedAdvancement = Adv
     { advancement :: Advancement   -- ^ Base advancement as entered by the user
     , baseSQ  :: Maybe XPType      -- ^ Base Source Quality
     , bonusSQ  :: XPType     -- ^ Bonus to Source Quality from Virtues and Flaws
     , scoreCap :: Maybe Int
     , levelLimit :: Maybe Int      -- ^ spell level allowance
     , spentXP  :: Maybe XPType     -- ^ Total XP spent on advancement
     , inferredTraits :: [ ProtoTrait ] -- ^ trait changes inferred by virtues and flaws
     , augYears :: Maybe Int        -- ^ number of years advanced
     , validation :: [Validation]   -- ^ Report from validation
     , postProcessTrait :: PostProcessor 
        -- ^ Extra postprocessing for traits at the given stage 
     , bookUsed :: [ Book ]    -- ^ Books required by the activity
     , teacherSQ :: Maybe Int
     }
   deriving (Eq,Show,Generic)

-- |
-- Type of function used to post-process traits after advancement.
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
     , baseSQ = Nothing
     , bonusSQ = 0
     , scoreCap = Nothing
     , levelLimit = Nothing 
     , spentXP = Nothing
     , inferredTraits = [ ] 
     , augYears = Nothing
     , validation = []
     , postProcessTrait = PostProcessor id
     , bookUsed = []
     , teacherSQ = Nothing
     }

instance Timed Advancement where
     season  = advSeason
instance StoryObject Advancement where
     name a = showTime xps (season a) (mode a) y 
         where xps | sx == Nothing = ""
                   | otherwise = " (" ++ ishow sx ++ "xp)" 
               sx = sourceQuality a
               ishow = showNum . fromJust
               y = advYears a
     narrative  = advNarrative
     comment  = advComment
instance AdvancementLike Advancement where
     mode = advMode
     usesBook = advUses
     sourceQuality  = advSQ
     sourceCap  = advCap
     changes = advChanges
instance Timed AugmentedAdvancement where
     season  = advSeason  .  advancement 

-- | Render the season and mode of an advancement
showTime :: String -> SeasonTime -> AdvancementType -> Maybe Int -> String
showTime xps NoTime tp y = (show tp ++ xps ++ showYears y)
showTime xps x tp y = (show x ++ xps ++ showYears y ++ " " ++ show tp)

-- | Render the duration of an advancement
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

instance StoryObject AugmentedAdvancement where
     name a = showTime xps (season a) (mode a) y 
         where xps = showSQ (sourceQuality a) (effectiveSQ a)
               y = augYears a
     narrative  = narrative . advancement
     comment  = comment . advancement

-- | Render the source quality of an advancement
showSQ :: Maybe XPType -> XPType -> String
showSQ Nothing 0 = " (0xp)"
showSQ (Just x) 0 = " (" ++ showNum x ++ "xp)"
showSQ Nothing (x) = " (" ++ showNum x ++ "xp)"
showSQ (Just x) (y) = " (" ++ showNum x ++ f (y-x) ++ "xp)"
    where f 0 = ""
          f z = "+" ++ showNum z
instance AdvancementLike AugmentedAdvancement where
     mode a = advMode  $ advancement a
     usesBook  a = advUses  $ advancement a
     sourceQuality  a =  advSQ  $ advancement a
     sourceCap  = scoreCap
     changes  a = advChanges  $ advancement  a

instance ToJSON AugmentedAdvancement where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AugmentedAdvancement where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .: "advancement"
        <*> v .:? "baseSQ"
        <*> v .:? "bonusSQ" .!= 0
        <*> v .:? "cap"
        <*> v .:? "levels"
        <*> v .:? "spentXP"
        <*> v .:? "inferredTraits"  .!= []
        <*> v .:? "augYears"
        <*> v .:?  "validation" .!= []
        <*> v .:? "postProcessTrait" .!= PostProcessor id
        <*> v .:? "bookUsed"  .!= []
        <*> v .:? "teacherSQ"  

effectiveSQ :: AugmentedAdvancement -> XPType
effectiveSQ aa = fromMaybe aasq adsq + vfb + inb
    where ad = advancement aa
          aasq = fromMaybe 0 $ baseSQ aa 
          adsq = sourceQuality ad
          vfb = bonusSQ aa
          inb = fromMaybe 0 $ advBonus ad
-- |
-- == Validation

-- |
-- A Validation is a message reporting either an error or a successful test.
data Validation = ValidationError String | Validated String
   deriving (Eq,Generic)

instance Show Validation where
    show (ValidationError x) = "ERROR: " ++ x
    show (Validated x) = "Validated: " ++ x

instance ToJSON Validation
instance FromJSON Validation


primaryXPTrait :: Advancement -> Maybe TraitKey
primaryXPTrait a | f a == [] = Nothing
                 | otherwise = Just $ traitKey $ head (f a)
   where f = sortOn ((*(-1)) . fromMaybe (-1) . xp) . filter (isJust . xp) . changes

-- | Sort the `inferredTraits` field of an `AugmentedAdvancement`.
sortInferredTraits :: AugmentedAdvancement -> AugmentedAdvancement
sortInferredTraits x = x { inferredTraits = sortTraits $ inferredTraits x }

