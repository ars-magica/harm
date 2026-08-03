{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Frontmatter
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Create frontmatter for an object
--
--
-----------------------------------------------------------------------------
module ArM.Markdown.Frontmatter where

import ArM.Types.Harm
import ArM.Story
import ArM.Sheet

class HarmObject h => Frontmatter h where
   frontmatter :: h -> [ String ]
   frontmatter _ = []
instance Frontmatter Character where
   frontmatter x = [ "---"
                   , "title: " ++ name x
                   , "---"
                   , "" ]
instance Frontmatter Covenant where
   frontmatter x = [ "---"
                   , "title: " ++ name x
                   , "---"
                   , "" ]
instance Frontmatter Library where
   frontmatter x = [ "---"
                   , "title: " ++ name x
                   , "---"
                   , "" ]
