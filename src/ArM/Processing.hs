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

import ArM.Story
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

-- | Add validation errors and notices to the covenant, i.e. to the
-- advancement currently being processed and stored at the head of past 
-- advancements.
addCovenantValidation :: [Validation] -> Covenant -> Covenant
addCovenantValidation val = updateCovenantAdv (addValidation val)


-- | Apply the given function to the CovenantState
updateCharacterAdv :: ( Augmented Advancement -> Augmented Advancement ) 
                  -> Character -> Character
updateCharacterAdv f s 
    | isNothing x = error "Updating non-existent covenant advancement"
    | otherwise = s { pastAdvancement = f (fromJust x):xs }
    where x = mhead $ pastAdvancement s
          xs = mtail $ pastAdvancement s

-- | Add validation errors and notices to the character, i.e. to the
-- advancement currently being processed and stored at the head of past 
-- advancements.
addCharacterValidation :: [Validation] -> Character -> Character
addCharacterValidation val = updateCharacterAdv (addValidation val)

-- | Replace the advancement currently being processed with the given one.
-- The current advancement is stored at the head of past advancements.
setAdvancement :: Augmented Advancement -> Character -> Character
setAdvancement aa ch = ch { pastAdvancement = aa:(mtail $ pastAdvancement ch) }

-- * Convenience functions (currently not in use)
--
-- $processing
-- Thes
--

-- | Extract abilities and arts from a list of `ProtoTrait` objects.
getAA :: [ ProtoTrait ] -> [ ProtoTrait ]
getAA = filter ( f . protoTrait )
    where f (AbilityKey _) = True
          f (ArtKey _) = True
          f (SpellKey _ _ _) = True
          f _ = False

-- | The covenant where the given character is a member
covenant :: Character -> Maybe HarmKey
covenant = fmap CovenantKey . memberOf 
