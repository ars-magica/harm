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

frontmatter :: HarmObject h => h -> [ String ]
frontmatter x = [ "---"
                , "title: " ++ stateName x
                , "authors:"
                , "- name: hArM"
                , "exports:"
                , "- format: pdf"
                , "  template: plain_latex"
                , "  output: " ++ fn ++ ".pdf"
                , "---"
                , "" ]
    where fn = fnFix $ stateName x

fnFix :: String -> String
fnFix = map f
   where f '(' = '-'
         f ')' = '-'
         f ' ' = '-'
         f x = x
