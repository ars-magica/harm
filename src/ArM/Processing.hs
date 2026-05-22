{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Processing
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Basic processing on the basic types
--
-- These functions update constituent elements of complex objects,
-- using the same principles for covenants and characters and sometimes
-- also sagas.
--
-- 1. update the state object
-- 2. update the current advancement
-- 3. add validation objects to the current advancement
--
-----------------------------------------------------------------------------
module ArM.Processing where


-- | Apply the given function to the SagaState
updateSagaState :: ( SagaState -> SagaState ) -> Saga -> Saga
updateSagaState f s = s { sagaState = f ( sagaState s ) }

-- | Apply the given function to the CovenantState
updateCovenantState :: ( CovenantState -> CovenantState ) -> Covenant -> Covenant
updateCovenantState f s = s { covenantState = fmap f ( covenantState s ) }

-- | Apply the given function to the CovenantState
updateCovenantAdv :: ( Augmented CovAdvancement -> Augmented CovAdvancement ) 
                  -> Covenant -> Covenant
updateCovenantAdv f s 
    | isNothing x = error "Updating non-existent covenant advancement"
    | otherwise = s { pastCovAdvancement = f (fromJust x):xs }
    where x = mhead $ pastCovAdvancement s
          xs = mtail $ pastCovAdvancement s

addCovenantValidation :: [Validation] -> Covenant -> Covenant
addCovenantValidation val = updateCovenantAdv (addValidation val)

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
                    ) where

import Data.List 

import ArM.Saga.Advancement
import ArM.Types.Harm
import ArM.Character
import ArM.Story
import Data.OList
import ArM.Helper

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
    where cvs = map cErrors $ characters saga
          covvs = map covErrors  $ covenants saga
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
lookupCharacters saga is = harmLookup is cs
    where cs = sortOnKey $ characters $ sagaState saga



-- | Apply the given function to the CovenantState
updateCharacterState :: ( CharacterState -> CharacterState ) -> Character -> Character
updateCharacterState f s = s { state = fmap f ( state s ) }

-- | Apply the given function to the CovenantState
updateCharacterAdv :: ( Augmented Advancement -> Augmented Advancement ) 
                  -> Character -> Character
updateCharacterAdv f s 
    | isNothing x = error "Updating non-existent covenant advancement"
    | otherwise = s { pastAdvancement = f (fromJust x):xs }
    where x = mhead $ pastAdvancement s
          xs = mtail $ pastAdvancement s

addCharacterValidation :: [Validation] -> Character -> Character
addCharacterValidation val = updateCharacterAdv (addValidation val)

