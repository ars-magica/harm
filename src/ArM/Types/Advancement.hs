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
-- + Augmented Advancement comprises the `Advancement` object and additional
--   inferred changes.
--
-- The Source Quality (SQ) is controled by several fields.
-- + Augmented Advancement `baseSQ` is the base source quality inferred
--   from other traits and general rules.
-- + Advancement `sourceQuality` allows the user to enter the basic
--   SQ manually.  This is required for Practice, and may be used to 
--   override the `baseSQ`.  If it differs from `baseSQ`, a warning
--   is issued.
-- + Augmented Advancement `bonusSQ` includes modifications derived
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
-- objects modifying existing traits.  Similarly, `Augmented Advancement`
-- has `inferredTraits` for additional implied changes.
--
-----------------------------------------------------------------------------
module ArM.Types.Advancement ( Advancement(..) 
                             , defaultAdvancement
                             , Augmented(..) 
                             , ContractAdvancement(..)
                             , AdvancementLike(..) 
                             , AdvancementType(..) 
                             , PostProcessor(..)
                             , BonusSQ(..)
                             -- * Covenant Advancement
                             , CovAdvancement(..)
                             , noCovAdvancement
                             -- * Validation
                             , Validation(..) 
                             , validateXP
                             , primaryXPTrait
                             ) where

import ArM.Helper
import ArM.Types.ProtoTrait
import ArM.Story
import ArM.Trait
import ArM.GameRules

import Data.Maybe 
import Data.List 
import Data.Char 
import Data.Aeson 
import Data.Aeson.Extra
import GHC.Generics
import Data.Text.Lazy                            ( fromStrict, unpack )
import Control.Monad

-- import ArM.Debug.Trace

-- * Advancement

-- ** Advancement Types

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

-- ** The Advancement Type 

-- | The advancement object has two roles.
-- It can hold the advancemet from one season or chargen stage,
-- as specified by the user.
-- It can also hold additional field inferred by virtues and flaws.
-- One may consider splitting these two functions into two types.
-- Note that standard SQ should be recorded as `advSQ`, while individual
-- variation may be recorded as `advBonus`.
data Advancement = Advancement
     { mode :: AdvancementType    -- ^ mode of study
     , advSeason :: SeasonTime    -- ^ season or development stage
     , years :: Maybe Int         -- ^ number of years advanced
     , advNarrative :: [ String ]    -- ^ narrative description of the activities
     , advComment :: [ String ]      -- ^ freeform description of the activities
     , requires :: [ HarmKey ]    -- ^ possessions required for exclusive use
     , readsBook :: [ HarmKey ]       -- ^ Books used exclusively by the character
     , usesBook :: [ String ]     -- ^ Books used exclusively by the character
     , readBook :: [ String ]     -- ^ Original book(s) read (to check against rereads)
     , bookUsed :: [ Book ]       -- ^ Books used exclusively by the character
     , sourceQuality :: Maybe XPType -- ^ Source Quality (SQ)
     , sourceCap :: Maybe Int        -- ^ Source Quality (SQ)
     , bonusSQ :: [ BonusSQ ]     -- ^ Bonus to Source Quality (SQ)
     , changes :: [ ProtoTrait ]  -- ^ trait changes defined by player
     , spellLevels :: Maybe Int   -- ^ spell level allowance
     , teacherSQ :: Maybe XPType  -- ^ The SQ generated as teacher
     , validation :: [Validation] -- ^ Report from validation
     , postprocessTrait :: PostProcessor -- ^ Extra postprocessing for traits at the given stage
     }
   deriving (Eq,Generic,Show)


-- | Default object for standardised initialisation of fields.
defaultAdvancement :: Advancement
defaultAdvancement = Advancement
     { mode = Exposure (OtherExposure "Undefined")
     , advSeason = NoTime
     , years = Nothing
     , advNarrative = []
     , advComment = []
     , requires = []
     , readsBook = []
     , usesBook = []
     , readBook = []
     , bookUsed = []
     , sourceQuality = Nothing
     , sourceCap = Nothing
     , bonusSQ = []
     , changes = []
     , spellLevels = Nothing
     , teacherSQ = Nothing
     , validation = []
     , postprocessTrait = PostProcessor id
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
        <*> v `parseCollapsedList` "requires"
        <*> v `parseCollapsedList` "reads"
        <*> v `parseCollapsedList` "usesBook"
        <*> v `parseCollapsedList` "readBook"
        <*> v `parseCollapsedList` "bookUsed"
        <*> v .:? "sourceQuality"
        <*> v .:? "sourceCap"
        <*> v `parseCollapsedList` "bonusQuality"
        <*> v `parseCollapsedList` "changes"
        <*> v .:? "spellLevels"
        <*> v .:? "teacherSQ"
        <*> v `parseCollapsedList` "validation"
        <*> v .:? "postProcessTrait" .!= PostProcessor id

instance Timed Advancement where
     season  = advSeason
instance StoryObject Advancement where
     name a = showTime (strSQ a) (season a) (mode a) (years a)
     narrative  = advNarrative
     comment  = advComment
     addNarrative s x = x { advNarrative = s:narrative x }
     addComment s x = x { advComment = s:comment x }

-- | Render the season and mode of an advancement
showTime :: String -> SeasonTime -> AdvancementType -> Maybe Int -> String
showTime xps NoTime tp y = (show tp ++ xps ++ showYears y)
showTime xps x tp y = (show x ++ xps ++ showYears y ++ " " ++ show tp)

-- | Render the duration of an advancement
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

-- | A Bonus with a description
data BonusSQ = BonusSQ 
         { sourceBonus :: XPType
         , bonusSource :: String
         }
         deriving (Eq,Generic,Show)
instance ToJSON BonusSQ
instance FromJSON BonusSQ



-- ** The AdvancementLike Class
class ContractAdvancement a where
    -- | Merge explicit and inferred advancement into one object
    contractAdvancement :: Augmented a -> a

instance ContractAdvancement Advancement where
    contractAdvancement ad = Advancement 
          { mode = ( mode . explicitAdv ) ad
          , advSeason = season ad
          , years = ( fmlx years ) ad
          , advNarrative = ( fmls narrative ) ad
          , advComment = ( fmls comment ) ad
          , usesBook = ( fmls usesBook ) ad
          , readBook = ( fmls readBook ) ad
          , bookUsed = ( fmls bookUsed ) ad
          , sourceQuality =  ( fmlx sourceQuality ) ad
          , sourceCap  = ( fmlx sourceCap ) ad
          , bonusSQ = ( fmls bonusSQ ) ad
          , changes = ( fmls changes ) ad
          , spellLevels = ( fmlx spellLevels ) ad
          , teacherSQ = ( fmlx teacherSQ ) ad
          , validation = ( fmls validation ) ad
          , postprocessTrait = ( postprocessTrait . inferredAdv ) ad
          }

-- |
-- The AdvancementLike class gives a common API to Advancement and
-- Augmented Advanceemnt
class (StoryObject a) => AdvancementLike a where
     -- | The type of advancement
     advMode :: a -> AdvancementType
     -- | Source Quality
     advSQ :: a -> Maybe XPType
     -- | Does the advancement give Exposure XP only?
     isExposure :: a -> Bool
     isExposure = f . advMode
        where f (Exposure _) = True
              f _ = False
     totalBonusSQ :: a -> XPType
     effectiveSQ :: a -> Maybe XPType
     -- | Sort the list of trait changes 
     sortAdvTraits :: a -> a
     -- | Count regular XP (excluding reputation) spent in an Advancement
     spentXP :: a -> XPType
     -- | Count spell levels from an Advancement
     spentLevels :: a -> Int
     addValidation :: [Validation] -> a -> a
     addProtoTrait :: [ProtoTrait] -> a -> a
     setRead :: BookDB h => h -> a -> a

instance AdvancementLike Advancement where
     advMode = mode
     advSQ = sourceQuality
     totalBonusSQ = sum . map sourceBonus . bonusSQ
     effectiveSQ a = fmap (+(totalBonusSQ a)) $ sourceQuality a 
     spentXP = sum . map regularXP . changes
     spentLevels = sum . map ( lvls . protoTrait ) . changes
         where lvls (SpellKey _ x _) = x
               lvls _ = 0
     sortAdvTraits x = x { changes = sortTraits $ changes x }
     addValidation vs a = a { validation = vs ++ validation a }
     addProtoTrait vs a = a { changes = vs ++ changes a }
     setRead _ ad = ad { readBook = map (bookID) (bookUsed ad) }

instance (Timed a, AdvancementLike a,ContractAdvancement a) 
       => AdvancementLike (Augmented a) where
     advMode = advMode . contractAdvancement
     advSQ = advSQ . contractAdvancement
     isExposure = isExposure . contractAdvancement
     totalBonusSQ = totalBonusSQ . contractAdvancement 
     effectiveSQ = effectiveSQ . contractAdvancement
     spentXP = spentXP . explicitAdv
     spentLevels = spentLevels . explicitAdv
     sortAdvTraits x = x { explicitAdv = sortAdvTraits $ explicitAdv x
                         , inferredAdv = sortAdvTraits $ inferredAdv x }
     addValidation vs a = a { inferredAdv = addValidation vs (inferredAdv a) }
     addProtoTrait vs a = a { inferredAdv = addProtoTrait vs (inferredAdv a) }
     setRead db ad = ad { inferredAdv = setRead db (inferredAdv ad) }


-- ** The Augmented Advancement

-- | Advancement with additional inferred information.
data Augmented a = Adv
     { explicitAdv :: a    -- ^ Explictly recorded (original) advancement
     , inferredAdv :: a    -- ^ Inferred advancement data
     }
   deriving (Eq,Show,Generic)

instance ToJSON a => ToJSON (Augmented a) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Augmented a) where
    parseJSON = withObject "AugmentedAdvancement" $ \v -> Adv
        <$> v .: "explicitAdv"
        <*> v .: "inferredAdv"

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


instance Timed a => Timed (Augmented a) where
     season  = season . explicitAdv 

instance (Timed a, ContractAdvancement a, AdvancementLike a, StoryObject a) 
      => StoryObject (Augmented a) where
    name = name . contractAdvancement
    narrative  = narrative . contractAdvancement
    comment  = comment . contractAdvancement
    addNarrative s (Adv a aa) = Adv a $ addNarrative s aa
    addComment s (Adv a aa) = Adv a $ addComment s aa

-- | Summarise SQ for display purposes
strSQ :: (AdvancementLike a) => a -> String
strSQ a = showSQ (advSQ a) (totalBonusSQ a)

-- | Render the source quality of an advancement
showSQ :: Maybe XPType -> XPType -> String
showSQ Nothing 0 = ""
showSQ (Just x) 0 = " (" ++ showNum x ++ "xp)"
showSQ Nothing (x) = " (" ++ showNum x ++ "xp)"
showSQ (Just x) (y) = " (" ++ showNum x ++ f y ++ "xp)"
    where f 0 = ""
          f z = "+" ++ showNum z

fmls :: (a -> [b]) -> Augmented a -> [b]
fmls f a = f (inferredAdv a) ++ f (explicitAdv a) 

fmlx :: Show b => (a -> Maybe b) -> Augmented a -> Maybe b
fmlx f aa = inf `mplus` exa
   where exa =  f (explicitAdv aa)
         inf =  f (inferredAdv aa)

-- ** Validation

-- |
-- A Validation is a message reporting either an error or a successful test.
data Validation = ValidationError String 
                | Validated String 
                | ValidationWarning String
   deriving (Eq,Generic)

instance Show Validation where
    show (ValidationError x) = "ERROR: " ++ x
    show (Validated x) = "Validated: " ++ x
    show (ValidationWarning x) = "Warning: " ++ x

instance ToJSON Validation
instance FromJSON Validation

-- | Find the trait earning the most XP from the advancement
primaryXPTrait :: Advancement -> Maybe TraitKey
primaryXPTrait = f' .  sortOn ((*(-1)) . fromMaybe (-1) . xp) . filter (isJust . xp) . changes
    where f' [] = Nothing
          f' (x:_) = Just $ traitKey x

-- | Validate allocation of XP.
validateXP :: Augmented Advancement -> Augmented Advancement
validateXP a = addValidation (xpValidation a) a

-- | Validate allocation of XP.
xpValidation :: Augmented Advancement -> [ Validation ]
xpValidation a 
    | isNothing sq' && xpsum > 0 = [ ValidationWarning $ "Undefined Source Quality. Spent " ++ showNum xpsum ++ "xp." ]
    | sq > xpsum = [ ValidationError $ "Underspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "." ]
    | sq < xpsum = [ ValidationError $ "Overspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "." ]
    | otherwise = [ Validated $ "Correctly spent " ++ showNum sq ++ " xp." ]
    where xpsum = spentXP a
          sq = fromMaybe 0 $ effectiveSQ a
          sq' =  effectiveSQ a

-- * Covenant Advancement 

-- | Advancement (changes) to a covenant.
data CovAdvancement = CovAdvancement 
     { caSeason :: SeasonTime    -- ^ season or development stage
     , caStory :: [ Story ]   -- ^ freeform description of the activities
     , caChanges :: [ ProtoTrait ]
     , joining :: [ HarmKey ]
     , leaving :: [ HarmKey ]
     , bookcsv :: Maybe String
     , acquired :: [ Possession ]
     , lost :: [ Possession ]
     , caType :: String
     } 
   deriving (Eq,Generic,Show)


-- | Empty `CovAdvancement` object for use as a default
noCovAdvancement :: CovAdvancement
noCovAdvancement = CovAdvancement NoTime [] [] [] [] Nothing [] [] "No Advancement"

instance ToJSON CovAdvancement
instance FromJSON CovAdvancement where
    parseJSON = withObject "CovAdvancement" $ \v -> CovAdvancement
        <$> fmap parseSeasonTime ( v .:? "season" )
        <*> v `parseCollapsedList` "story" 
        <*> v `parseCollapsedList` "changes" 
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "joining" )
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "leaving" )
        <*> v .:? "bookcsv"
        <*> v `parseCollapsedList` "acquired"
        <*> v `parseCollapsedList` "lost"
        <*> v .:? "type" .!= "In Game"

instance Timed CovAdvancement where
   season = caSeason

instance ContractAdvancement CovAdvancement where
  contractAdvancement aug  = CovAdvancement
     { caSeason = season aug
     , caStory = caStory aa ++ caStory ad
     , caChanges = caChanges aa ++ caChanges ad
     , joining = joining aa ++ joining ad
     , leaving = leaving aa ++ leaving ad
     , bookcsv = bookcsv aa
     , acquired = acquired aa ++ acquired ad
     , lost = lost aa ++ lost ad
     , caType = caType aa
     } 
     where (Adv aa ad) = aug
