{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Spell
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  MarkDown output for Spells, Lab Texts, and Magic Effects.
--
-- This is an internal module.  Relevant definitions are intended to be 
-- exposed from "ArM.Markdown".
--
-----------------------------------------------------------------------------
module ArM.Markdown.Spell where

import ArM.Trait
import ArM.Story
import ArM.GameRules
import ArM.Helper
import ArM.Markdown.HList
import Data.OList
import Data.HList
-- import Data.Maybe

import ArM.Debug.Trace

-- | Render the lab text in markdown HList format.
textH :: LabText -> HList
textH (SpellText x) = spellH x
textH (Device x) = effectH x

-- | Render the 'MagicEffect in markdown HList format.
effectH :: MagicEffect -> HList
effectH x = HList ( spellSignatureMD x ) ( effectDetails x )

-- | Render the 'SpellRecord' in markdown HList format.
spellH :: SpellRecord -> HList
spellH x = HList ( spellSignatureMD x ) ( map f $ spellDetails x )
    where f y = HList y []

-- | Render the lab text in markdown OList format.
textMD :: LabText -> OList
textMD = fromHList . textH

-- | Render the details of a 'MagicEffect' (auxiliary for 'textH').
effectDetails :: MagicEffect -> [ HList ]
effectDetails x = filter (not . isEmptyHList) $ filterNothing $ map ($ x ) ls
  where ls = [ maybeHList . requirements
             , maybeHList . showRDT
             , maybeHList . showStrList . effectModifiers
             , effectMP "Description" . map italic . narrative
             , maybeHList . trs . effectTrigger
             , effectMP "Comment" . comment
             , maybeHList . effectDesign
             , maybeHList . effectReference ]
        trs "" = ""
        trs y = "Trigger: " ++ y


-- | Render the details of a 'SpellRecord' (auxiliary for 'spellH').
spellDetails :: SpellRecord -> [ String ]
spellDetails sp = filter (/="") $ map ($ sp) ls
  where ls = [ requirements, spellStats, design, cite ]

-- | List the spell stats, incl. range/duration/target and any
-- special tags like ritual.
spellStats :: SpellRecord -> String
spellStats sp = (showRDT sp) ++ spstr
   where spstr | [] == specialSpell sp = ""
               | otherwise = "; " ++ showStrList (specialSpell sp)

-- | Show any requisites for the effect.
requirements :: SpellLike a => a -> String
requirements sp | req == [] = ""
                | otherwise = "Req. " ++ showStrList req
   where req = reqTechnique sp ++ reqForm sp

-- | Show range/duration/target of the spell or effect
showRDT :: SpellLike a => a -> String
showRDT sp = "Range: " ++ r ++
             "; Duration: " ++ d ++
             "; Target: " ++ t
   where (r,d,t) = rdt sp

-- | Render a spell trait in Markdown
-- The result should normally be subject to indentList to make an hierarchical
-- list.
spellDescH :: (Spell,Maybe SpellRecord) -> HList
spellDescH (s,Nothing) = trace "No SpellRecord" $ spellDescH' s
spellDescH (s,Just y) = HList "" [ spellDescH' s, coreSpellRecordH y ]
spellDescH' :: Spell -> HList
spellDescH' s = HList ( show s ) 
             $ filterNothing ( masteryH s:map jhlist ( spellTComment s ) )

-- | Render the spell record as an HList
coreSpellRecordH :: SpellRecord -> HList
coreSpellRecordH sp = HList "" $ filterNothing hs
    where hs = nh:ch:(map jhlist $ spellDetails sp)
          nh = narrativeH sp
          ch = commentH sp

-- | Set all information from mastery on one line.
-- This includes mastery score, xp, and mastery options.
masteryH :: Spell -> Maybe HList
masteryH s | 0 == masteryScore s && 0 == spellExcessXP s = Nothing
           | otherwise = jhlist
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ showNum (spellExcessXP s) ++ "xp) "
                          ++ showStrList (masteryOptions s)

-- | Set all information from mastery on one line.
-- This includes mastery score, xp, and mastery options.
masteryMD :: Spell -> OList
masteryMD s | 0 == masteryScore s && 0 == spellExcessXP s = OList []
            | otherwise = OString
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ showNum (spellExcessXP s) ++ "xp) "
                          ++ showStrList (masteryOptions s)

