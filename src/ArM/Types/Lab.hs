{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Lab
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent the laboratory and sanctum of a magus
--
--
-----------------------------------------------------------------------------
module ArM.Types.Lab where

import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.List

import ArM.Helper

import ArM.Debug.Trace


data Lab = Lab 
         { labName :: String
         , labDescription :: String
	 , labState :: LabState
         , pastLabAdvancement :: [ LabAdvancement ]
         , futureLabAdvancement :: [ LabAdvancement ]
       }  deriving (Eq,Generic,Show)
instance ToJSON Lab
instance FromJSON Lab
    parseJSON = withObject "Covenant" $ \v -> Covenant
        <$> v .: "name"
        <*> v .:? "description" .!= ""
        <*> v .:? "state" .!= defaultLabState
        <*> v .:? "history" .!= []
        <*> v .:? "plan" .!= []

-- | ID of a Lab, to reference it independently of the state.
data LabID = LabID String
    deriving ( Show, Ord, Eq, Generic )

instance ToJSON LabID
instance FromJSON LabID

-- | get the ID of a character.
labID :: Lab -> LabID
labID = LabID . labName

instance HarmObject Lab where
    name = labName 
    stateSeason = fromMaybe NoTime . fmap labTime . labState
    prepare x = id

-- |
-- = LabState Object

data LabState = LabState 
         { labTime :: SeasonTime
         , labVirtues :: [ LabVirtue ]
         , labRefinement :: Int 
         , labSize :: Int 
       }  deriving (Eq,Generic,Show)

defaultLabState :: LabState 
defaultLabState = LabState 
         { labTime = GameStart
         , labVirtues = []
         , labRefinement = 0
         , labSize = 0
       }  


instance ToJSON LabState
instance FromJSON LabState where
    parseJSON = withObject "LabState" $ \v -> LabState
        <$> v .:? "season" .!= GameStart
        <*> v .:? "virtues" .!= []
        <*> v .:? "refinement" .!= 0
        <*> v .:? "size" .!= 0

data LabVirtue = LabVirtue 
     { labVirtueName :: String
     , labVirtueDetail :: String
     , labVirtueDescription :: String
     , labVirtueBonus :: [ LabBonus ]
     , labVirtueMechanics :: String
     }  deriving (Eq,Generic,Show)

instance ToJSON LabVirtue
instance FromJSON LabVirtue where
    parseJSON = withObject "LabVirtue" $ \v -> LabVirtue
        <$> v .: "name" 
        <*> v .:? "detail" .!= ""
        <*> v .:? "description" .!= ""
        <*> v .:? "bonus" .!= []
        <*> v .:? "mechanics" .!= ""

data LabBonus = LabBonus 
     { labTrait :: String
     , labSpecialisation :: String
     , labScore :: Int
     }  deriving (Eq,Generic,Show)

instance ToJSON LabBonus
instance FromJSON LabBonus where
    parseJSON = withObject "LabBonus" $ \v -> LabBonus
        <$> v .: "name" 
        <*> v .:? "specialisation" .!= ""
        <*> v .:? "score" .!= 0

-- |
-- = Advancement and Traits

-- | Advancement (changes) to a covenant.
data LabAdvancement = LabAdvancement 
     { labSeason :: SeasonTime    -- ^ season or development stage
     , labNarrative :: [String]   -- ^ freeform description of the activities
     , acquireVirtue :: [ LabVirtue ]
     , loseVirtue :: [ LabVirtue ]
     , newSize :: Maybe Int
     , labRefine :: Int
     }
   deriving (Eq,Generic,Show)
defaultAdv :: LabAdvancement 
defaultAdv = LabAdvancement 
     { labSeason = NoTime
     , labNarrative = []
     , acquireVirtue = []
     , loseVirtue = []
     , newSize = Nothung
     , labRefine = 0
     }
instance ToJSON CovAdvancement
instance FromJSON CovAdvancement where
    parseJSON = withObject "CovAdvancement" $ \v -> CovAdvancement
        <$> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "narrative" .!= []
        <*> v .:? "joining" .!= []
        <*> v .:? "leaving" .!= []
        <*> v .:? "acquired" .!= []
        <*> v .:? "lost" .!= []


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
applyCovAdv (c,Nothing) = (c,Nothing)
applyCovAdv (c,Just a) = (c',Just a)
    -- where (a',st') = applyCovAdvancement a st
    where st' = st { covTime = caSeasonAug a, covenFolkID = cid }
          c' = c { covenantState = Just st' }
          st = fromMaybe defaultCovState $ covenantState c
          cid1 = sort $ joiningAug a ++ covenFolkID st 
          cid = cid1 -= ( sort $ leavingAug a )


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

contractAdvancement :: AugCovAdvancement -> CovAdvancement
contractAdvancement aug  = CovAdvancement
     { caSeason = season aug
     , caNarrative = listFromMaybe caNarrative aa ++ listFromMaybe caNarrative ad
     , joining = listFromMaybe joining aa ++ listFromMaybe joining ad
     , leaving = listFromMaybe leaving aa ++ listFromMaybe leaving ad
     , acquired = listFromMaybe acquired aa ++ listFromMaybe acquired ad
     , lost = listFromMaybe lost aa ++ listFromMaybe lost ad
     } 
     where (AugCovAdvancement aa ad) = aug

