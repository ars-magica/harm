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
module ArM.Saga ( characterIndex
                    , covenantIndex
                    , advancementErrors
                    , advancementWarnings
                    , advancementErrorsLimit
                    , covenFolk
                    -- * Advancement
                    , advanceSaga 
                    , Advance(..)
                    , Validation(..)
                    -- * Convenience
                    , characterList
                    , covenantList
                    ) where

import ArM.Saga.Advancement
import ArM.Saga.Saga
