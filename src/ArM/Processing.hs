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

import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Helper
import Data.Maybe


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

setCharacterState :: CharacterState -> Character -> Character
setCharacterState st ch = ch { state = Just st }
setAdvancement :: Augmented Advancement -> Character -> Character
setAdvancement aa ch = ch { pastAdvancement = aa:(mtail $ pastAdvancement ch) }
