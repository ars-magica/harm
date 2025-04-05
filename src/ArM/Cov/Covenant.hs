{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Covenant
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
module ArM.Cov.Covenant where

import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.List

import ArM.Char.Character
import ArM.Types.Library
-- import ArM.Helper

-- import ArM.Debug.Trace


-- |
-- = Covenant Object

-- | A Covenant consists of a state and a timeless concept, as well as
-- lists of advancement which define the evolution of states
data Covenant = Covenant 
         { covenantConcept :: CovenantConcept
         , covenantState :: Maybe CovenantState
         , pastCovAdvancement :: [ CovAdvancement ]
         , futureCovAdvancement :: [ CovAdvancement ]
       }  deriving (Eq,Generic,Show)
instance ToJSON Covenant 
instance FromJSON Covenant where
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "concept"
        <*> v .:? "state"
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []

-- | ID of a Covenant.
-- This is currently implemented as the name.
-- It is used to reference the covenant, independtly of state, from other
-- objects.
data CovenantID = CovenantID String
    deriving ( Show, Ord, Eq, Generic )

instance ToJSON CovenantID
instance FromJSON CovenantID

-- | get the ID of a character.
covenantID :: Covenant -> CovenantID
covenantID = CovenantID . name


instance HarmObject Covenant where
    name = covName . covenantConcept
    stateSeason = fromMaybe NoTime . fmap covTime . covenantState

-- |
-- = CovenantConcept Object

data CovenantConcept = CovenantConcept 
         { covName :: String
         , covConcept :: Maybe String
         , covFounded :: Maybe Int
         , covAppearance :: Maybe String
         , covTribunal :: Maybe String
         , covData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) covenant concept object.
defaultCovConcept :: CovenantConcept 
defaultCovConcept = CovenantConcept { covName = "Player Covenant"
                                  , covConcept = Nothing
                                  , covFounded = Nothing
                                  , covAppearance = Nothing
                                  , covTribunal = Nothing
                                  , covData = KeyPairList []
       }  

instance ToJSON CovenantConcept where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CovenantConcept where
    parseJSON = withObject "CovenantConcept" $ \v -> CovenantConcept
        <$> v .: "name"
        <*> v .:? "concept"
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
         , covenFolkID :: [ CharacterID ]
         , library :: [ BookCopy ]
       }  deriving (Eq,Generic,Show)


instance ToJSON CovenantState
instance FromJSON CovenantState


-- |
-- = Advancement and Traits

-- | Advancement (changes) to a covenant.
data CovAdvancement = CovAdvancement 
     { caSeason :: SeasonTime    -- ^ season or development stage
     , caNarrative :: Maybe String -- ^ freeform description of the activities
     , joining :: [ CharacterID ]
     , leaving :: [ CharacterID ]
     , acquired :: [ BookCopy ]
     , lost :: [ BookCopy ]
     }
   deriving (Eq,Generic,Show)

defaultAdv :: CovAdvancement 
defaultAdv = CovAdvancement 
     { caSeason = NoTime
     , caNarrative = Nothing
     , joining = []
     , leaving = []
     , acquired = []
     , lost = []
     }
instance ToJSON CovAdvancement
instance FromJSON CovAdvancement


-- |
-- The `Advance` instance is very similar to that of `Character`, but has to
-- be implemented separately to account for different advancement classes.
instance Advance Covenant where
   advance ct c | futureCovAdvancement c == [] = c
                | isNothing (covenantState c) = advance ct $ prepare c
                | ct < ct' = c
                | otherwise =  advance ct $ step c 
            where y =  head $ futureCovAdvancement c
                  ct' =  caSeason y
   step c = c { covenantState = Just cs 
              , pastCovAdvancement = (a:xs)
              , futureCovAdvancement = ys 
              }
            where (y:ys) = futureCovAdvancement c
                  xs = pastCovAdvancement c
                  (a,cs) = applyCovAdvancement y cstate
                  cstate = fromJust $ covenantState c
   nextSeason = f . futureCovAdvancement
       where f [] = NoTime
             f (x:_) = caSeason x

-- | Apply advancement
applyCovAdvancement :: CovAdvancement
                 -> CovenantState 
                 -> (CovAdvancement,CovenantState)
applyCovAdvancement a cs = (a,cs')
    where cs' = cs { covTime = caSeason a
                   , covenFolkID = sort $ joining a ++ covenFolkID cs' }
{-
          new = advanceTraitList change tmp
          tmp = sortTraits $ advanceTraitList inferred old 
          change = sortTraits $ inferDecrepitude $ changes a'
          old = sortTraits $ traits cs
-}
