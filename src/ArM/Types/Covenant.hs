{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Covenant
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Types.Covenant ( 
           -- * The Covenant Type
           Covenant(..)
           , CovenantConcept(..)
           , CovenantState(..)
           , defaultCovState
           -- * Advancement
           , CovAdvancement(..)
           , AugCovAdvancement(..)
           , contractAdvancement
           -- * Convenience Functions
           , findCov
           , covenant
           ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Maybe
import Control.Monad

import ArM.Types.Library
import ArM.Types.Character
import ArM.Types.Lab
import ArM.Types
import ArM.Types.Possession
import ArM.Helper

-- * Covenant Tybe

-- | A Covenant consists of a state and a timeless concept, as well as
-- lists of advancement which define the evolution of states
data Covenant = Covenant 
         { covenantConcept :: CovenantConcept
         , covenantState :: Maybe CovenantState
         , pastCovAdvancement :: [ AugCovAdvancement ]
         , futureCovAdvancement :: [ CovAdvancement ]
       }  deriving (Eq,Generic,Show)
instance Timed Covenant where
    season = fromMaybe NoTime . fmap covTime . covenantState

instance ToJSON Covenant 
instance FromJSON Covenant where
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "concept"
        <*> v .:? "state"
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []

instance BookDB Covenant where
   lookupBook k = join . fmap (lookupBook k) . covenantState 
instance BookDB CovenantState where
   lookupBook k = lookupBook k . library

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
         , library :: [ Book ]
            -- ^ The covenant library.  This should be deprecated and
            -- replaced by a function extracting books from possessions,
            -- since Book is a special case of Possession.
         , librarycsv :: Maybe String    
            -- ^ File to be loaded into the library
            -- This should only be used temporarily, when loading the state
            -- from JSON.
         , possessions :: [ Possession ]
         , labs :: [ Lab ]
       }  deriving (Eq,Generic,Show)

-- | A default object with some fields pre-initialised.
defaultCovState :: CovenantState 
defaultCovState = CovenantState 
         { covTime = GameStart
         , covenFolkID = []
         , library = []
         , librarycsv = Nothing
         , possessions = []
         , labs = []
       }  


instance ToJSON CovenantState
instance FromJSON CovenantState where
    parseJSON = withObject "CovenantState" $ \v -> CovenantState
        <$> v .:? "season" .!= GameStart
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "covenfolk" )
        <*> v `parseCollapsedList` "library"
        <*> v .:? "librarycsv"
        <*> v `parseCollapsedList` "possessions"
        <*> v `parseCollapsedList` "labs"


-- * Advancement and Traits

-- | Advancement (changes) to a covenant.
data CovAdvancement = CovAdvancement 
     { caSeason :: SeasonTime    -- ^ season or development stage
     , caStory :: [ Story ]   -- ^ freeform description of the activities
     , joining :: [ HarmKey ]
     , leaving :: [ HarmKey ]
     , acquired :: [ Book ]
     , lost :: [ Book ]
     }
   deriving (Eq,Generic,Show)

instance ToJSON CovAdvancement
instance FromJSON CovAdvancement where
    parseJSON = withObject "CovAdvancement" $ \v -> CovAdvancement
        <$> fmap parseSeasonTime ( v .:? "season" )
        <*> v `parseCollapsedList` "story" 
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "joining" )
        <*> fmap ( map CharacterKey ) ( v `parseCollapsedList` "leaving" )
        <*> v `parseCollapsedList` "acquired"
        <*> v `parseCollapsedList` "lost"


-- |
-- Augmented advancement for covenants.  This comprises changes inferred
-- from the advancements of characters (and possibly other covenants)
-- as well as the advancement from the input file.
data AugCovAdvancement = AugCovAdvancement 
     { explicit :: Maybe CovAdvancement
     , inferred :: Maybe CovAdvancement
     }
   deriving (Eq,Generic,Show)
instance ToJSON AugCovAdvancement
instance FromJSON AugCovAdvancement where

instance Timed AugCovAdvancement where
   season = fromMaybe NoTime . fmap caSeason . explicit
instance Timed CovAdvancement where
   season = caSeason

-- | Merge explicit and inferred advancement into onw `CovAdvancement` object
contractAdvancement :: AugCovAdvancement -> CovAdvancement
contractAdvancement aug  = CovAdvancement
     { caSeason = season aug
     , caStory = listFromMaybe caStory aa ++ listFromMaybe caStory ad
     , joining = listFromMaybe joining aa ++ listFromMaybe joining ad
     , leaving = listFromMaybe leaving aa ++ listFromMaybe leaving ad
     , acquired = listFromMaybe acquired aa ++ listFromMaybe acquired ad
     , lost = listFromMaybe lost aa ++ listFromMaybe lost ad
     } 
     where (AugCovAdvancement aa ad) = aug

-- | Auxiliary for `contractAdvancement`.  Apply a map which return
-- a list, returning an empty list for a Nothing argument.
listFromMaybe :: ( a -> [b]) -> Maybe a -> [b]
listFromMaybe f = fromMaybe [] . fmap f 

-- * Convenience Functions

-- |
-- Find the character's covenant from a list.
-- The covenant is identified by checking if the character is
-- listed as a member (covenFolkID).
findCov :: Character -> [Covenant] -> Maybe Covenant
findCov ch cs = maybeHead xs
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

