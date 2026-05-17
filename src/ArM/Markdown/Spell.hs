{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Spell
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  MarkDown output for Possession.
--
-- This is an internal module.
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
textH (SpellText x) = HList ( spellSignatureMD x ) ( map f $ spellDetails x )
    where f y = HList y []
textH (Device x) = HList ( spellSignatureMD x ) ( effectDetails x )

-- | Render the lab text in markdown OList format.
textMD :: LabText -> OList
textMD = fromHList . textH


effectDetails :: MagicEffect -> [ HList ]
effectDetails x = filter (not . isEmptyHList) $ filterNothing $ map ($ x ) ls
  where ls = [ Just . toHList' . requirements
             , Just . toHList' . showRDT
             , effectMP "Description" . map italic . narrative
             , effectMP "Comment" . comment
             , Just . toHList' . effectDesign
             , Just . toHList' . effectReference ]

effectMP :: String -> [String] -> Maybe HList 
effectMP _ [] = Nothing
effectMP _ [x] = Just $ HList x []
effectMP h xs = Just $ HList h $ map toHList' xs

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



-- | Render a magic effect declaration in Markdown
printEffectMD :: MagicEffect -> OList
printEffectMD ob = OList
       [ OString $ name ob ++ " (" ++ teforql ob ++ ")"
       , OList [ OString $ effectRDT ob
       , nonemptyStringMD $ showStrList md
       , trs
       ]
       , OList $ map italicOString $ narrative ob
       , OList $ map OString $ comment ob
       , OList $ [  OString $ show $ effectDesign ob 
       , nonemptyStringMD $ effectReference ob 
       ]
       ]
       where tr = effectTrigger ob
             trs | tr == "" = OList []
                 | otherwise = OString $ "Trigger: " ++ tr
             md = effectModifiers ob

