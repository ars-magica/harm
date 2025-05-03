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
                    , characterIndex
                    , covenantIndex
                    , advancementErrors
                    , advancementErrorsLimit
                    , covenFolk
                    ) where

-- import Data.Maybe 
import Data.List 

import ArM.Advancement
import ArM.Types.Covenant
import ArM.Char.Character
import ArM.Types
import ArM.Types.Saga
import ArM.BasicIO
import ArM.Helper

-- import ArM.Debug.Trace

-- |
-- == Error reports

-- | Get an `OList` of all error messages from past advancements in a
-- saga state.
--
-- CharGen errors are only included at GameStart and ignored later.
advancementErrors :: SagaState -> OList
advancementErrors saga | errors == [] = OString "No errors"
                       | otherwise = OList $ map formatOutput errors
    where formatOutput (cid,_,ssn,vs) = 
              headOList ( cid ++ ": " ++ ssn ) (mapmsg vs ) 
          errors = errorList saga
          mapmsg [] = []
          mapmsg ((ValidationError x):xs) = x:mapmsg xs
          mapmsg (_:xs) = mapmsg xs

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
augHead' :: SeasonTime -> AdvancementType -> String
augHead' NoTime tp = show tp
augHead' x tp = (show x  ++ " " ++ show tp)


-- | 
-- == Character Index

-- | Write a single item for `characterIndex`
characterIndexLine :: Character -> OList
characterIndexLine c = OString $ "+ " ++ pagesLink (stateName c) 

-- | Write a bullet list of links for a list of characters
characterIndex :: [Character] -> OList
characterIndex = OList . map characterIndexLine 

-- | Write a single item for `covenantIndex`
covenantIndexLine :: Covenant -> OList
covenantIndexLine c = OString $ "+ " ++ pagesLink (stateName c) 

-- | Write a bullet list of links for a list of characters
covenantIndex :: [Covenant] -> OList
covenantIndex = OList . map covenantIndexLine 

-- |
-- == Covenant support

-- |
-- List of covenFolk as `Character` objects at the covenant
covenFolk :: Saga -> CovenantState -> [ Character ]
covenFolk saga cov = lookupCharacters s $ f cov
   where f = covenFolkID 
         s = saga

-- |
-- Find `Character` objects for a list of character IDs, from the given `Saga`.
lookupCharacters :: Saga -> [ HarmKey ] -> [ Character ]
lookupCharacters saga is = harmLookup is cs
    where cs = sortOnKey $ characters $ sagaState saga

