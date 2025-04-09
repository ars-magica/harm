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
import ArM.Helper

import ArM.Debug.Trace


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

covenant :: CharacterState -> Maybe CovenantID
covenant = fmap CovenantID . memberOf 

instance HarmObject Covenant where
    name = covName . covenantConcept
    stateSeason = fromMaybe NoTime . fmap covTime . covenantState
    prepare x = trace "prepare Covenant" $ f x
        where f y | isNothing (covenantState y) = y { covenantState = Just defaultCovState }
                  | otherwise = y 

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
        <*> v .:? "covenfolk" .!= []
        <*> v .:? "library" .!= []
        <*> v .:? "librarycsv"


-- |
-- = Advancement and Traits

-- | Advancement (changes) to a covenant.
data CovAdvancement = CovAdvancement 
     { caSeason :: SeasonTime    -- ^ season or development stage
     , caNarrative :: String     -- ^ freeform description of the activities
     , joining :: [ CharacterID ]
     , leaving :: [ CharacterID ]
     , acquired :: [ Book ]
     , lost :: [ Book ]
     }
   deriving (Eq,Generic,Show)
defaultAdv :: CovAdvancement 
defaultAdv = CovAdvancement 
     { caSeason = NoTime
     , caNarrative = ""
     , joining = []
     , leaving = []
     , acquired = []
     , lost = []
     }
instance ToJSON CovAdvancement
instance FromJSON CovAdvancement where
    parseJSON = withObject "CovAdvancement" $ \v -> CovAdvancement
        <$> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "narrative" .!= ""
        <*> v .:? "joining" .!= []
        <*> v .:? "leaving" .!= []
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


-- |
-- The `Advance` instance is very similar to that of `Character`, but has to
-- be implemented separately to account for different advancement classes.
instance Advance Covenant where
   advance ct c | isNothing (covenantState c) = trace "need prepare" $ advance ct $ prepare c
                | ct < ct' =  c
                | otherwise =  advance ct $ step c 
            where ct' =  nextSeason c

   stepIf ns = trace ("stepIf (Cov): "++show ns) $ completeCovAdv . applyCovAdv . nextCovAdv ns

   step cov = completeCovAdv $ applyCovAdv (new,Just y')
            where (y:ys) = futureCovAdvancement cov
                  y' = prepareAdvancement y
                  new = cov { futureCovAdvancement = ys }
   nextSeason = f . futureCovAdvancement
       where f [] = NoTime
             f (x:_) = caSeason x

-- | CovenFolk joining according to the augmented covenant advancement.
joiningAug :: AugCovAdvancement -> [CharacterID]
joiningAug (AugCovAdvancement a b) = a' ++ b'
    where a' = fromMaybe [] $ fmap joining a
          b' = fromMaybe [] $ fmap joining b

-- | CovenFolk leaving according to the augmented covenant advancement.
leavingAug :: AugCovAdvancement -> [CharacterID]
leavingAug (AugCovAdvancement a b) = a' ++ b'
    where a' = fromMaybe [] $ fmap leaving a
          b' = fromMaybe [] $ fmap leaving b

-- | Get the season of the augmented covenant advancement.
-- This is taken from the explicit advancement if available, and
-- the inferred advancement otherwise.
caSeasonAug :: AugCovAdvancement -> SeasonTime
caSeasonAug (AugCovAdvancement a b) = fromMaybe b' $ fmap caSeason a
    where b' = fromMaybe NoTime $ fmap caSeason b

-- | Apply covenant advancement
applyCovAdv :: (Covenant,Maybe AugCovAdvancement)
         -> (Covenant,Maybe AugCovAdvancement)
applyCovAdv (c,Nothing) = trace (stateName c ++ " - Nothing") $ (c,Nothing)
applyCovAdv (c,Just a) = trace (stateName c ++ " - " ++ show (caSeasonAug a)) $ (c',Just a)
    -- where (a',st') = applyCovAdvancement a st
    where st' = st { covTime = caSeasonAug a, covenFolkID = cid }
          c' = c { covenantState = Just st' }
          st = fromMaybe defaultCovState $ covenantState c
          cid1 = sort $ joiningAug a ++ covenFolkID st 
          cid = cid1 -= ( sort $ leavingAug a )

-- |
-- Find the character's covenant from a list.
-- The covenant is identified by checking if the character is
-- listed as a member (covenFolkID).
findCov :: Character -> [Covenant] -> Maybe Covenant
findCov ch cs | xs == [] = Nothing
              | otherwise = Just $ head xs
    where xs = filter (`hasMember` ch) cs

-- |
-- Does the covenant have the character as a member?
hasMember :: Covenant -> Character -> Bool
hasMember cov ch = cid `elem` chs
   where cid = characterID ch
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
              | caSeason adv > ns = trace (show (ns,caSeason adv,covenantID cov)) $ (cov,Nothing)
              | otherwise = (new,Just a)
        where a = prepareAdvancement adv
              (adv:as) = fs
              fs = futureCovAdvancement cov
              new = cov { futureCovAdvancement = as }


prepareAdvancement :: CovAdvancement -> AugCovAdvancement
prepareAdvancement a = AugCovAdvancement (Just a) Nothing
