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
import ArM.Types.Story
import ArM.GameRules
import ArM.Helper
import Data.OList
import Data.Maybe

-- | Render the spell record as an OList
coreSpellRecordMD :: Maybe SpellRecord -> OList
coreSpellRecordMD Nothing = OList []
coreSpellRecordMD sr = OList [ reqstr
                             , OString $ (showRDT sp) ++ spstr
                             , os (spellDescription sp)
                             , os (design sp)
                             , os (cite sp)
                             ]
   where req = techniqueReq sp ++ formReq sp
         sp = fromJust sr
         os "" = OList []
         os x = OString x
         reqstr | req == [] = OList []
                | otherwise = OString $ "Req. " ++ showStrList req
         spstr | [] == specialSpell sp = ""
               | otherwise = "; " ++ showStrList (specialSpell sp)

showRDT :: SpellRecord -> String
showRDT sp = "Range: " ++ r ++
             "; Duration: " ++ d ++
             "; Target: " ++ t
   where (r,d,t) = rdt sp

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
       [ OString $ name ob ++ effectTeFo ob
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

effectTeFo :: MagicEffect -> String
effectTeFo eff = " (" ++ te ++ rt ++ fo ++ rf ++ show (effectLevel eff) ++ ")"
   where te = take 2 $ effectTechnique eff
         fo = take 2 $ effectForm eff
         rts = effectTechniqueReq eff
         rfs = effectFormReq eff
         rt | rts == [] = ""
            | otherwise = foldl (++) "" $ map (take 2) rts
         rf | rts == [] = ""
            | otherwise = foldl (++) "" $ map (take 2) rfs

