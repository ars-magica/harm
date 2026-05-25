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

import ArM.Markdown.HOutput
import ArM.Trait
import ArM.Story
import ArM.Helper
import Data.HList

-- | Render a 'VF' object
vfH :: VF -> HList 
vfH ob = HList n hs
   where hs = filterNothing [ f (count ob), narrativeH ob , commentH ob ]
         n = name ob ++ " (" ++ show (vfcost ob) ++ ")"
         f 1 = Nothing
         f x = Just $ HList ( "Taken " ++ show x ++ " times." ) []
