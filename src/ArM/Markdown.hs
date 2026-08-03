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
-- The different pages to produce are
-- + Annals, use `sagaAnnals`
-- + Character
-- + Covenant
-- + SagaA
-- + Library
--
-- There are only four functions exported.  The other functions are
-- internal.
--
-- Top-level functions are entitites with their own pages:
-- + Saga top level page $\to$ `printSheetMD`
-- + Saga state (per season) $\to$ `sagaStateMD`
-- + Character $\to$ `printSheetMD`
-- + Covenatn $\to$ `printSheetMD`
--
-----------------------------------------------------------------------------
module ArM.Markdown ( printMD, printSheetMD
                    , sagaStateMD
                    , sagaAnnals
                    , HOutput
                    , frontmatter
                    ) where


import ArM.Markdown.HOutput 
import ArM.Markdown.Frontmatter 
import ArM.Markdown.SeasonDisplay

import ArM.Types.Harm

import Control.Monad.State.Lazy
import Data.Maybe

import Data.OList
import Data.HList

-- * The Markdown class

-- | This is the basic function to render in Markdown
printMD :: HOutput h => h -> OList
printMD = fromMaybe (OString "") . fmap fromHList . printH

-- | This is a hack to augment characters using extra resources
-- By default, it is identical to 'printMD'.
printSheetMD :: HOutput h => Saga -> h -> OList
printSheetMD saga x = fromMaybe (OList []) 
                    $ fmap fromHList 
                    $ evalState ( printS x ) saga

-- | Render the state page for the Saga
sagaStateMD :: Saga -> OList 
sagaStateMD = fromHList . sagaStateH


