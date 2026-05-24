{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Classes and instances to make MarkDown output.
--
-- The core of this module is the 'Markdown' class and its 'printMD'
-- function which renders an object in Markdown.  There is also 
-- a 'LongSheet' class with a 'printSheetMD' function for a more
-- verbose character sheet.
--
-----------------------------------------------------------------------------
module ArM.Markdown ( Markdown(..)
                    , artMD
                    , artVisMD
                    , italicOString
                    , storyOList
                    , enchantedMD  -- Unused
                    , sagaStateMD
                    -- * SeasonDisplay
                    , sagaAnnals
                    , AnnalSeason(..)
                    ) where


import ArM.Markdown.Markdown 
import ArM.Markdown.SeasonDisplay
