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
import Data.OList
import Data.HList
-- import Data.Maybe



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

-- | Render comment and narrative of 'MagicEffect'
-- (auxiliary for 'effectDetails').
effectMP :: String -> [String] -> Maybe HList 
effectMP _ [] = Nothing
effectMP _ [x] = Just $ HList x []
effectMP h xs = Just $ HList h $ map ( \ s -> HList s [] ) xs

-- | Render the details of a 'SpellRecord' (auxiliary for 'spellH').
spellDetails :: SpellRecord -> [ String ]
spellDetails sp = filter (/="") $ map ($ sp) ls
  where ls = [ requirements, spellStats, spellDescription, spellComment, design, cite ]

-- | Enclose the string in asterixes, indicating italics in Markdown.
-- The idea is to be able to override this function for other output formats.
italic :: String -> String
italic "" = ""
italic x = "*" ++ x ++ "*"

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

-- | Render the spell record as an OList
coreSpellRecordMD :: Maybe SpellRecord -> OList
coreSpellRecordMD Nothing = OList []
coreSpellRecordMD (Just sp) = OList $ map OString $ spellDetails sp

-- | Render a spell trait in Markdown
-- The result should normally be subject to indentOList to make an hierarchical
-- list.
spellDescMD :: (Spell,Maybe SpellRecord) -> OList
spellDescMD (s,sr) = OList [ OString $ show s
                  , OList [ masteryMD s, f $ spellTComment s ]
                  , coreSpellRecordMD sr
                  ]
     where f "" = OList [] 
           f x = OString x

-- | Set all information from mastery on one line.
-- This includes mastery score, xp, and mastery options.
masteryMD :: Spell -> OList
masteryMD s | 0 == masteryScore s && 0 == spellExcessXP s = OList []
            | otherwise = OString
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ showNum (spellExcessXP s) ++ "xp) "
                          ++ showStrList (masteryOptions s)



