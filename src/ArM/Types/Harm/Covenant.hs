{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Harm.Covenant
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Covenants and functions for advancement.
--
-- Covenants use the `CovAdvancement` type from `ArM.Types.Advancement` with
-- some code shared with characters.  The code to advance a single step, as
-- well as the `covGen` function advancing to Game Start, are included here.
--
-- InGame advancement must be done jointly for all characters and covenants,
-- and this is handled by the `ArM.Advancement` module.
--
-----------------------------------------------------------------------------
module ArM.Types.Harm.Covenant ( 
           -- * The Covenant Type
           Covenant(..)
           , CovenantConcept(..)
           , CovenantState(..)
           , defaultCovState
           , findCov
           , covenant
           ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Maybe
import Control.Monad

import ArM.Trait
import ArM.Types.Advancement
import ArM.Types.Harm.Character
import ArM.Story
import ArM.Helper
import Data.KeyPair

-- * Covenant Tybe

-- | A Covenant consists of a state and a timeless concept, as well as
-- lists of advancement which define the evolution of states
data Covenant = Covenant 
         { covenantConcept :: CovenantConcept
         , covenantState :: Maybe CovenantState
         , pastCovAdvancement :: [ Augmented CovAdvancement ]
         , futureCovAdvancement :: [ CovAdvancement ]
         , covenantDesign :: [ CovAdvancement ]
         , covenantPregame :: [ Augmented CovAdvancement ]
       }  deriving (Eq,Generic,Show)
instance Timed Covenant where
    season = fromMaybe NoTime . fmap season . covenantState

instance Timed CovenantState where
    season = covTime

instance ToJSON Covenant 
instance FromJSON Covenant where
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "concept"
        <*> v .:? "state"
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []
        <*> v .:? "design" .!= []
        <*> v .:? "pregame" .!= []

instance BookDB Covenant where
   lookupBook k = join . fmap (lookupBook k) . covenantState 

instance BookDB CovenantState where
    lookupBook k = lookupBook k . library

library :: CovenantState -> [ Book ]
library = foldl (++) [] . map bookTexts . filter isBook . possessions


instance KeyObject Covenant where
    harmKey = CovenantKey . name

instance HarmObject Covenant 
instance StoryObject Covenant where
    name = covName . covenantConcept
    narrative = covDescription . covenantConcept

-- | The covenant concept is the timeless features of the covenant,
-- as compared to the `CovenantState` which advances over time.
data CovenantConcept = CovenantConcept 
         { covName :: String
         , covConcept :: Maybe String
         , covDescription :: [ String ]
         , covFounded :: Maybe Int
         , covAppearance :: Maybe String
         , covTribunal :: Maybe String
         , covData :: KeyPairList
       }  deriving (Eq,Generic)

instance ToJSON CovenantConcept where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CovenantConcept where
    parseJSON = withObject "CovenantConcept" $ \v -> CovenantConcept
        <$> v .: "name"
        <*> v .:? "concept"
        <*> v .:? "description" .!= []
        <*> v .:? "founded"
        <*> v .:? "appearance"
        <*> v .:? "tribunal"
        <*> v .:? "data" .!= KeyPairList []

instance Show CovenantConcept where
   show c = covName c ++ " covenant (est. " ++ sf (covFounded c) ++ ") "
         ++ (fromMaybe "" $ covConcept c) ++ "\n"
         ++ ( show $ covData c )
    where sf Nothing = "-"
          sf (Just x ) = show x


-- | The `CovenantState` is the features of the covenant that changes
-- from season to season
data CovenantState = CovenantState 
         { covTime :: SeasonTime
         , covenFolkID :: [ HarmKey ]
         , caTraits :: [ OtherTrait ]
         , boonhook :: [ VF ]
         , possessions :: [ Possession ]
         , labs :: [ Lab ]
       }  deriving (Eq,Generic,Show)

-- | A default object with some fields pre-initialised.
defaultCovState :: CovenantState 
defaultCovState = CovenantState 
         { covTime = GameStart
         , covenFolkID = []
         , caTraits = []
         , boonhook = []
         , possessions = []
         , labs = []
       }  


instance ToJSON CovenantState
instance FromJSON CovenantState where
    parseJSON = withObject "CovenantState" $ \v -> CovenantState
        <$> v .:? "season" .!= GameStart
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "covenfolk" )
        <*> v `parseCollapsedList` "traits"
        <*> v `parseCollapsedList` "boonhook"
        <*> v `parseCollapsedList` "possessions"
        <*> v `parseCollapsedList` "labs"


-- * Convenience Functions

-- |
-- Find the character's covenant from a list.
-- The covenant is identified by checking if the character is
-- listed as a member (covenFolkID).
findCov :: Character -> [Covenant] -> Maybe Covenant
findCov ch cs = mhead xs
    where xs = filter (`hasMember` ch) cs

-- |
-- Does the covenant have the character as a member?
hasMember :: Covenant -> Character -> Bool
hasMember cov ch = cid `elem` chs
   where cid = harmKey ch
         chs = fromMaybe [] $ fmap covenFolkID $ covenantState cov


-- | The covenant where the given character is a member
covenant :: CharacterState -> Maybe HarmKey
covenant = fmap CovenantKey . memberOf 
