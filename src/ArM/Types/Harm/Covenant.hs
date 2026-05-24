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
           , findCov
           ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Maybe

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
         , covTime :: SeasonTime
         , covenFolkID :: [ HarmKey ]
         , caTraits :: [ OtherTrait ]
         , boonhook :: [ VF ]
         , possessions :: [ Possession ]
         , labs :: [ Lab ]
         , pastCovAdvancement :: [ Augmented CovAdvancement ]
         , futureCovAdvancement :: [ CovAdvancement ]
         , covenantDesign :: [ CovAdvancement ]
         , covenantPregame :: [ Augmented CovAdvancement ]
       }  deriving (Eq,Generic,Show)

instance Timed Covenant where
    season = covTime

instance ToJSON Covenant 
instance FromJSON Covenant where
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "concept"
        <*> v .:? "season" .!= GameStart
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "covenfolk" )
        <*> v `parseCollapsedList` "traits"
        <*> v `parseCollapsedList` "boonhook"
        <*> v `parseCollapsedList` "possessions"
        <*> v `parseCollapsedList` "labs"
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []
        <*> v .:? "design" .!= []
        <*> v .:? "pregame" .!= []

instance BookDB Covenant where
   lookupBook k = lookupBook k . possessions

instance KeyObject Covenant where
    harmKey = CovenantKey . name

instance HarmObject Covenant 
instance StoryObject Covenant where
    name = covName . covenantConcept
    narrative = covDescription . covenantConcept

-- | The covenant concept is the timeless features of the covenant,
-- as compared to the state which advances over time.
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
hasMember cov ch = (harmKey ch) `elem` (covenFolkID cov)
   


