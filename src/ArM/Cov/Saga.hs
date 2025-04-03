{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Cov.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
                    , advanceSaga
                    , sagaStateName
                    , characterIndex
                    , covenantIndex
                    , advancementErrors
                    , advancementErrorsLimit
                    ) where

-- import Data.Maybe 
import Data.Aeson 
import Data.List 
import GHC.Generics

import ArM.Char.Character
import ArM.DB.Spell
import ArM.DB.Weapon
import ArM.Cov.Covenant
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Saga type

-- | A Saga as it is processed in memory.
-- Multiple files have to be loaded to generate a Saga object from a `SagaFile`.
data Saga = Saga 
         { sagaTitle :: String
         , sagaStates :: [ SagaState ]
         , rootDir :: String
         , sagaDesc :: String
         , baseURL :: Maybe String
         , spells :: SpellDB
         , weapons :: WeaponDB
         , armour :: ArmourDB
       }  deriving (Eq)

-- | Get the most recent SagaState
-- This is mainly for backward compatibility
sagaState :: Saga -> SagaState
sagaState = head . sagaStates

instance Show Saga where
   show saga = "Saga: " ++ sagaTitle saga

-- | Saga state at a particular point in time, comprising characters and
-- covenants at that point.
data SagaState = SagaState 
         { stateTitle :: String
         , seasonTime :: SeasonTime
         , covenants :: [Covenant]
         , characters :: [Character]
         }  deriving (Eq,Show)


-- | Get the name of the Saga as recorded in the SagaState
sagaStateName :: SagaState -> String
sagaStateName s = stateTitle s ++ " - " ++ (show $ seasonTime s)

-- |
-- == SagaFile object

-- | A Saga as it is stored on file.
-- The main purpose here is to identify all the files used for characters and
-- other data in the saga.
data SagaFile = SagaFile 
         { title :: String
         , seasons :: [ SeasonTime ]
         , currentSeason :: SeasonTime
         , rootDirectory :: Maybe String
         , sagaDescription :: String
         , covenantFiles :: [String]
         , characterFiles :: [String]
         , spellFile :: String
         , weaponFile :: String
         , armourFile :: String
       }  deriving (Eq,Generic,Show)

instance ToJSON SagaFile 
instance FromJSON SagaFile where
    parseJSON = withObject "SagaFile" $ \v -> SagaFile
       <$> v .: "title"
       <*> v .:? "seasons" .!= []
       <*> v .:? "currentSeason" .!= NoTime
       <*> v .:? "rootDirectory" 
       <*> v .:? "description"  .!= ""
       <*> v .:? "covenantFiles" .!= []
       <*> v .:? "characterFiles" .!= []
       <*> v .:? "spellFile" .!= "spells.csv"
       <*> v .:? "weaponFile" .!= "weapons.csv"
       <*> v .:? "armourFile" .!= "armour.csv"


-- |
-- == Error reports

-- | Get an `OList` of all error messages from past advancements in a
-- saga state.
--
-- CharGen errors are only included at GameStart and ignored later.
advancementErrors :: SagaState -> OList
advancementErrors saga | errors == [] = OString "No errors"
                       | otherwise = OList $ map formatOutput errors
    where formatOutput (cid,_,ssn,vs) = OList 
              [ OString ( cid ++ ": " ++ ssn ),
              OList $ map msg vs ]
          errors = errorList saga
          msg (ValidationError x) = OString x
          msg _ = OString ""

-- | Convenience type for a list of validation messages for a 
-- given cvharacter and season
type VList = (String,SeasonTime,String,[Validation])

-- | Did the `VList` object occur after the given season?
errorAfter :: VList -> SeasonTime -> Bool
errorAfter (_,vs,_,_) s = vs > s 

-- | Exctract all validation errors from previous advancements at
-- a given saga state
errorList :: SagaState -> [VList]
errorList saga = sortOn ( \ (_,x,_,_) -> x ) vvs
    where cs = characters saga
          cvs = map cErrors  cs
          vvs = g cvs 
          g = f . filterVList . foldl (++) [] 
          f [] = []
          f ((_,_,_,[]):xs) = f xs
          f (x:xs) = x:f xs

-- | Extract only errors from a list of `VList` objects.
filterVList :: [VList] -> [VList]
filterVList = map ( \ (a,b,c,d) -> (a,b,c,filterError d) ) 

-- | Get errors from a list of Validation objects
filterError :: [Validation] -> [Validation]
filterError (Validated _:xs) = filterError xs
filterError (x:xs) = x:filterError xs
filterError [] = []

-- | Exctract a list of validation errors after a given time 
-- This is not currently used, but could be used to ignore old
-- errors when reporting recent character states.
advancementErrorsLimit :: SeasonTime ->  SagaState -> OList
advancementErrorsLimit ssn saga = OList $ map formatOutput errors
    where formatOutput (cid,_,sn,vs) = OList 
              [ OString ( cid ++ ": " ++ sn ),
              OList $ map msg vs ]
          errors = f $ errorList saga
          msg (ValidationError x) = OString x
          msg _ = OString ""
          f [] = []
          f (x:xs) | x `errorAfter` ssn = x:f xs
                   | otherwise = []

-- | Get validation messages from a given advancement.
-- Auxiliary for `cErrors`
aaErrors :: Character -> AugmentedAdvancement -> VList
aaErrors c a = (charID c, season a, augHead a, vs )
    where vs = validation  a

-- | Get validation messages from a given character.
-- Auxiliary for `listErrors`
cErrors :: Character -> [VList]
cErrors c = map (aaErrors c) as
   where as | ps == [] = pregameDesign c
            | otherwise = ps
         ps = pastAdvancement c

-- | Format a header for `renderCharErrors`
augHead :: AugmentedAdvancement -> String
augHead a = augHead' (season a) (mode a)
-- | Format a header for `renderCharErrors`
augHead' :: SeasonTime -> Maybe String -> String
augHead' NoTime Nothing = ("??" )
augHead' x Nothing = (show x )
augHead' NoTime (Just x) = x
augHead' x (Just z) = (show x  ++ " " ++ z)


-- | Character Index
-- ==

-- | Write a single item for `characterIndex`
characterIndexLine :: Character -> OList
characterIndexLine c = OString $ "+ " ++ pagesLink (characterStateName c) 

-- | Write a bullet list of links for a list of characters
characterIndex :: [Character] -> OList
characterIndex = OList . map characterIndexLine 

-- | Write a single item for `covenantIndex`
covenantIndexLine :: Covenant -> OList
covenantIndexLine c = OString $ "+ " ++ pagesLink (covenantStateName c) 

-- | Write a bullet list of links for a list of characters
covenantIndex :: [Covenant] -> OList
covenantIndex = OList . map covenantIndexLine 

-- |
-- = Advancement

instance Advance Saga where
   -- advance :: SeasonTime -> a -> a
   advance t saga = saga { sagaStates = x:sagaStates saga }
      where x = advance t (sagaState saga)

   -- step :: a -> a
   step saga = saga { sagaStates = x:sagaStates saga }
      where x = step (sagaState saga)

   -- nextSeason :: a -> SeasonTime
   nextSeason = f . sagaStates
       where f [] = NoTime
             f (x:_) = nextSeason x


-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: SagaFile -> Saga -> Saga
advanceSaga t saga = advanceSaga' (seasons t) saga

advanceSaga' :: [SeasonTime] -> Saga -> Saga
advanceSaga' [] = id
advanceSaga' (x:xs) = trace ("adv> " ++ show x) $ advanceSaga' xs . advance x 


instance Advance SagaState where
   advance t saga 
      | NoTime == ns = saga 
      | t < ns = saga 
      | otherwise = advance t $ step saga
     where ns = nextSeason saga

   step saga = saga { stateTitle = stateTitle saga 
                    , seasonTime = ns
                    , covenants = map (advance ns) $ covenants saga
                    , characters = map (advance ns) $ characters saga 
                    }
     where ns = nextSeason saga

   nextSeason saga = foldl min NoTime ss
      where ss = [ nextSeason x | x <- characters saga ]
