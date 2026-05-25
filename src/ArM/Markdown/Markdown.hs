{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Markdown
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
module ArM.Markdown.Markdown ( printMD, printSheetMD
                    , sagaStateMD
                    ) where

import ArM.Markdown.HOutput 
import ArM.Markdown.Possession 
import ArM.Character 
import ArM.Types.Harm
import ArM.Story
import ArM.Sheet
import ArM.Trait
import ArM.Helper

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
printSheetMD saga x = fromMaybe (OList []) $ fmap fromHList 
                      $ evalState ( printS x ) saga

-- ** Top level entities
--
-- $markdown
-- Top-level functions are entitites with their own pages:
-- + Saga top level page $\to$ `printSheetMD`
-- + Saga state (per season) $\to$ `sagaStateMD`
-- + Character $\to$ `printSheetMD`
-- + Covenatn $\to$ `printSheetMD`
--

-- | Render the state page for the Saga
sagaStateMD :: Saga -> OList 
sagaStateMD = fromHList . sagaStateH


-- * Markdown for the Character types
 
instance HOutput Enchantment  where
   printH (LesserItem eff) = printH eff 
   printH (GreaterDevice vn eff) = Just $ HList 
       ( "Greater Enchanted Device (opened with " ++ show vn ++ "p vis)" )
       ( filterNothing $ map printH eff )
   printH (Talisman vn eff) = Just $ HList 
       ( "Talisman (opened with " ++ show vn ++ "p vis)" )
       ( filterNothing $ map printH eff )
   printH (ChargedItem vn eff) = Just $ HList 
       ( "Charged Item (" ++ show vn ++ "charges)" )
       ( filterNothing [ printH eff ] )
   printH MundaneItem = jhlist "Mundane Item" 

enchantedMD :: Possession -> Enchantment -> OList
enchantedMD _ MundaneItem = OList []
enchantedMD ob (LesserItem eff) = OList 
         [ OString $ pName ob ++ tf ++ "lesser enchanted device"
                   , f $ printMD eff ]
   where f (OList xs) = foldOList $ OList $ mtail xs
         f os = os
         tf = " (" ++ teforql eff ++ ") "

enchantedMD ob enc = OList [ OString $ pName ob 
                          , printMD $ enc
                           ]


