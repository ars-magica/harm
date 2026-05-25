{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.HList
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Rendering data as 'HList' objects.
--
-- This will hopefully be easier to program than the existing
-- 'OList' approach.  The 'fromHList' function converts to 'OList',
-- so existing IO functions can be used.
--
-----------------------------------------------------------------------------
module ArM.Markdown.HList where

import Data.HList
import ArM.Markdown.Spell
import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Story
import ArM.Helper
import Data.Maybe
import ArM.Debug.Trace

-- | Render the narrative comment.
narrativeH :: StoryObject a => a -> Maybe HList
narrativeH = effectMP "Background" . map italic . narrative

-- | Render the comment.
commentH :: StoryObject a => a -> Maybe HList
commentH = effectMP "Comment" . comment

paragraphsH :: [ String ] -> HList
paragraphsH = HList "" . map (\x -> HList "" [ hlist x ] )

-- | Make a Maybe HLIst from a string
jhlist :: String -> Maybe HList
jhlist = Just . hlist

-- | Render a description list item
dlH :: String -> String -> HList
dlH x y = HList x [ hlist (':':' ':y), hlist "" ]

-- | Render a description list item
dlMaybeH :: String -> Maybe String -> HList
dlMaybeH x y = HList x [ hlist (':':' ':fromMaybe "---" y), hlist "" ]

-- | Write a bullet list of links for a list of characters
characterIndexH :: [Character] -> HList
characterIndexH = HList "+ Characters" . map f 
    where f = hlist . ("    + "++) . pagesLink . stateName 
-- | Write a bullet list of links for a list of characters
covenantIndexH :: [Covenant] -> HList
covenantIndexH = HList "+ Covenants" . map f 
    where f = hlist . ("    + "++) . pagesLink . stateName 

