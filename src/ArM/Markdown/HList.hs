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
import ArM.Trait
import ArM.Story

-- | Render the narrative comment.
narrativeH :: StoryObject a => a -> Maybe HList
narrativeH = effectMP "Background" . map italic . narrative

-- | Render the comment.
commentH :: StoryObject a => a -> Maybe HList
commentH = effectMP "Comment" . comment

class HOutput h where
   -- | Render an object in 'HList' format allowing markdown notation.
   printH :: h -> HList

instance HOutput LabText where
   printH = textH

instance HOutput SpellRecord where
   printH = spellH
instance HOutput MagicEffect where
   printH = effectH
