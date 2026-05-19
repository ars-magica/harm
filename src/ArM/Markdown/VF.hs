{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.VF
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Rendering virtues and flaws, boons and hooks.
--
-- This is an internal module.
--
-----------------------------------------------------------------------------
module ArM.Markdown.VF where

import ArM.Markdown.HList
import ArM.Trait
import ArM.Story
import ArM.Helper
import Data.HList

import ArM.Debug.Trace

-- | Render a 'VF' object
vfH :: VF -> HList 
vfH ob = HList (ttrace $ name ob) hs
   where hs = filterNothing [ narrativeH ob , commentH ob ]
