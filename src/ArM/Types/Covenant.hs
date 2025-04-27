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
module ArM.Types.Covenant ( Covenant(..)
                          , CovenantConcept(..)
                          , CovenantState(..)
                          , CovAdvancement(..)
                          , AugCovAdvancement(..)
                          , nextCovAdv
                          , prepareCovAdvancement
                          , findCov
                          , completeCovAdv
                          , defaultCovState
                          , findBook
                          , contractAdvancement
                          , covenant
                          ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Maybe

import ArM.Types.Library
import ArM.Types.Character
import ArM.Types
import ArM.Helper

-- |
-- = Covenant Object

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


instance KeyObject Covenant where
    harmKey = CovenantKey . name

covenant :: CharacterState -> Maybe HarmKey
covenant = fmap CovenantKey . memberOf 

instance HarmObject Covenant 
instance StoryObject Covenant where
    name = covName . covenantConcept
    narrative = covDescription . covenantConcept

-- |
-- = CovenantConcept Object

data CovenantConcept = CovenantConcept 
         { covName :: String
         , covConcept :: Maybe String
         , covDescription :: [ String ]
         , covFounded :: Maybe Int
         , covAppearance :: Maybe String
         , covTribunal :: Maybe String
         , covData :: KeyPairList
       }  deriving (Eq,Generic)

{-
-- | Default (empty) covenant concept object.
defaultCovConcept :: CovenantConcept 
defaultCovConcept = CovenantConcept { covName = "Player Covenant"
                                  , covConcept = Nothing
                                  , covDescription = [ ]
                                  , covFounded = Nothing
                                  , covAppearance = Nothing
                                  , covTribunal = Nothing
                                  , covData = KeyPairList []
       }  
-}

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


-- |
-- = CovenantState Object

data CovenantState = CovenantState 
         { covTime :: SeasonTime
         , covenFolkID :: [ HarmKey ]
         , library :: [ Book ]
         , librarycsv :: Maybe String
       }  deriving (Eq,Generic,Show)

defaultCovState :: CovenantState 
defaultCovState = CovenantState 
         { covTime = GameStart
         , covenFolkID = []
         , library = []
         , librarycsv = Nothing
       }  


instance ToJSON CovenantState
instance FromJSON CovenantState where
    parseJSON = withObject "CovenantState" $ \v -> CovenantState
        <$> v .:? "season" .!= GameStart
        <*> fmap ( map CharacterKey ) ( v .:? "covenfolk" .!= [] )
        <*> v .:? "library" .!= []
        <*> v .:? "librarycsv"


-- |
-- = Advancement and Traits

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

{-
defaultAdv :: CovAdvancement 
defaultAdv = CovAdvancement 
     { caSeason = NoTime
     , caStory = []
     , joining = []
     , leaving = []
     , acquired = []
     , lost = []
     }
-}

instance ToJSON CovAdvancement
instance FromJSON CovAdvancement where
    parseJSON = withObject "CovAdvancement" $ \v -> CovAdvancement
        <$> fmap parseSeasonTime ( v .:? "season" )
        <*> v `parseCollapsedList` "story" 
        <*> fmap ( map CharacterKey ) ( v .:? "joining" .!= [] )
        <*> fmap ( map CharacterKey ) ( v .:? "leaving" .!= [] )
        <*> v .:? "acquired" .!= []
        <*> v .:? "lost" .!= []


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

-- |
-- Find the character's covenant from a list.
-- The covenant is identified by checking if the character is
-- listed as a member (covenFolkID).
findCov :: Character -> [Covenant] -> Maybe Covenant
findCov ch cs = maybeHead xs
    where xs = filter (`hasMember` ch) cs

findBook ::  CovenantState -> String -> Maybe Book
findBook cov bs = maybeHead xs
    where xs = filter ( (==bs) . bookID ) ( library cov )

-- |
-- Does the covenant have the character as a member?
hasMember :: Covenant -> Character -> Bool
hasMember cov ch = cid `elem` chs
   where cid = harmKey ch
         chs = fromMaybe [] $ fmap covenFolkID $ covenantState cov


-- | Complete the advancement procedure to return the new Covenant with
-- the updated state.
completeCovAdv :: (Covenant,Maybe AugCovAdvancement)
                 -> Covenant
completeCovAdv (c,Nothing) = c
completeCovAdv (c,Just a) = c { pastCovAdvancement = a:pastCovAdvancement c }

-- |
-- Get the next augmented advancement.
nextCovAdv :: SeasonTime -> Covenant -> (Covenant,Maybe AugCovAdvancement)
nextCovAdv ns cov | fs == [] = (cov,Nothing)
              | caSeason adv > ns = (cov,Nothing)
              | otherwise = (new,Just a)
        where a = prepareCovAdvancement adv
              (adv:as) = fs
              fs = futureCovAdvancement cov
              new = cov { futureCovAdvancement = as }


prepareCovAdvancement :: CovAdvancement -> AugCovAdvancement
prepareCovAdvancement a = AugCovAdvancement (Just a) Nothing

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

listFromMaybe :: ( a -> [b]) -> Maybe a -> [b]
listFromMaybe f = fromMaybe [] . fmap f 
