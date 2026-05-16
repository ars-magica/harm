{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Fundamental types widely used
--
-- This module contains exports the types to process characters and 
-- advancement, including persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Types ( module Data.KeyPair
                 , module ArM.Types.Lab
                 ) where

import Data.KeyPair
import ArM.Types.Lab
