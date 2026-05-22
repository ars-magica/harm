{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
                    , characterIndex
                    , covenantIndex
                    , advancementErrors
                    , advancementNotices
                    , advancementErrorsLimit
                    , covenFolk
                    -- * Advancement
                    , advanceSaga 
                    , Advance(..)
                    , StepAdvance(..)
                    , Validation(..)
                    -- * Convenience
                    , characterList
                    , covenantList
                    ) where

import Data.List 

import ArM.Saga.Advancement
import ArM.Types.Harm
import ArM.Character
import ArM.Story
import ArM.Helper

import Data.OList
import qualified Data.Map as M

characterList = M.elems . characters
covenantList = M.elems . covenants

-- |
-- == Error reports

-- | Get an `OList` of all error messages from past advancements in a
-- saga state.
--
-- CharGen errors are only included at GameStart and ignored later.
advancementErrors :: SagaState -> OList
advancementErrors = advancementE isValError

advancementE :: (Validation->Bool) -> SagaState -> OList
advancementE f saga | errors == [] = OString "No errors"
                       | otherwise = OList $ map formatOutput errors
    where formatOutput (cid,_,ssn,vs) = 
              headOList ( show cid ++ ": " ++ ssn ) (map show $ filter f vs)
          errors = errorList saga

-- | Get an `OList` of all non-error validation messages from past advancements in a
-- saga state.
--
-- CharGen notices are only included at GameStart and ignored later.
advancementNotices :: SagaState -> OList
advancementNotices = advancementE (not . isValError)

isValError :: Validation -> Bool
isValError (ValidationError _) = True
isValError _ = False

-- | Convenience type for a list of validation messages for a 
-- given cvharacter and season
type VList = (HarmKey,SeasonTime,String,[Validation])

-- | Did the `VList` object occur after the given season?
errorAfter :: VList -> SeasonTime -> Bool
errorAfter (_,vs,_,_) s = vs > s 

-- | Exctract all validation errors from previous advancements at
-- a given saga state
errorList :: SagaState -> [VList]
errorList saga = sortOn ( \ (_,x,_,_) -> x ) vvs
    where cvs = map cErrors $ characterList saga
          covvs = map covErrors  $ covenantList saga
          vvs = g (cvs ++ covvs)
          g = f . foldl (++) [] 
          f [] = []
          f ((_,_,_,[]):xs) = f xs
          f (x:xs) = x:f xs

-- | Exctract a list of validation errors after a given time 
-- This is not currently used, but could be used to ignore old
-- errors when reporting recent character states.
advancementErrorsLimit :: SeasonTime ->  SagaState -> OList
advancementErrorsLimit ssn saga = OList $ map formatOutput errors
    where formatOutput (cid,_,sn,vs) = OList 
              [ OString ( show cid ++ ": " ++ sn ),
              OList $ map msg vs ]
          errors = f $ errorList saga
          msg (ValidationError x) = OString x
          msg _ = OString ""
          f [] = []
          f (x:xs) | x `errorAfter` ssn = x:f xs
                   | otherwise = []

-- | Get validation messages from a given advancement.
-- Auxiliary for `cErrors`
aaErrors :: ContractAdvancement a => HarmKey -> Augmented a -> VList
aaErrors c a = (c, season a, augHead a, vs )
    where vs = validation  a

-- | Get validation messages from a given character.
-- Auxiliary for `listErrors`
cErrors :: Character -> [VList]
cErrors c = map (aaErrors $ harmKey c) as
   where as | ps == [] = pregameDesign c
            | otherwise = ps
         ps = pastAdvancement c

-- | Get validation messages from a given covenant.
-- Auxiliary for `listErrors`
covErrors :: Covenant -> [VList]
covErrors c = map (aaErrors $ harmKey c) as
   where as | ps == [] = covenantPregame c
            | otherwise = ps
         ps = pastCovAdvancement c

-- | Format a header for `renderCharErrors`
augHead :: ContractAdvancement a => Augmented a -> String
augHead a = f (season a) (advancementmode a)
   where f NoTime tp = tp
         f x tp = (show x  ++ " " ++ tp)


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
lookupCharacters saga is = filterNothing $ map ( \ i -> harmLookup i cs ) is
    where cs = sagaState saga

