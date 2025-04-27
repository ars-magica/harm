{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains exports the types to process characters and 
-- advancement, including persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Char.Character ( module ArM.Types.Character
                          , module ArM.Types.Advancement
                          , Advancement(..)
                          , characterEntryTime
                          ) where

-- import Data.Maybe 
-- import Data.List
-- import Control.Monad

import ArM.Types.Advancement
import ArM.Types
-- import ArM.Types.Library
import ArM.Types.Character
-- import ArM.Char.Validation
-- import ArM.GameRules

-- |
-- = Convenience Functions for Character Properties

-- | The first season the character is played
characterEntryTime :: Character -> SeasonTime
characterEntryTime c | tm == NoTime = f $ futureAdvancement c
                     | otherwise = tm
     where tm = entryTime c
           f [] = tm
           f (x:_) = season x

