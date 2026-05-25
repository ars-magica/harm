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
module ArM.Markdown.Markdown ( Markdown(..)
                    , italicOString
                    , storyOList
                    , sagaStateMD
                    , enchantedMD  -- Unused
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

-- | Default implementation of `printMD` in the markdown class.
defaultMD :: HOutput h => h -> OList
defaultMD = fromMaybe (OString "") . fmap fromHList . printH
-- | Default implementation of `printSheetMD` in the markdown class.
defaultSheetMD :: HOutput h => Saga -> h -> OList
defaultSheetMD saga x = fromMaybe (OList []) $ fmap fromHList 
                      $ evalState ( printS x ) saga

-- | Class defining 'printMD' to render in Markdown.
class Markdown a where
     -- | This is the basic function to render in Markdown
     printMD :: a           -- ^ object to render
             -> OList       -- ^ list of lines for output

     -- | This is a hack to augment characters using extra resources
     -- By default, it is identical to 'printMD'.
     printSheetMD :: Saga      -- ^ Saga including databases for spells etc.
                -> a         -- ^ object to render
                -> OList     -- ^ list of lines for output
     printSheetMD _ = printMD

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


instance Markdown Saga where
    printMD = defaultMD
instance Markdown Character where
   printMD _ = error "Character printMD is not supported"
   printSheetMD = defaultSheetMD
instance Markdown Covenant where
    printMD = defaultMD
    printSheetMD = defaultSheetMD


-- * Markdown for the Character types
 
instance Markdown MagicEffect  where
   printMD = defaultMD 
instance Markdown Enchantment  where
   printMD (LesserItem eff) = printMD eff 
   printMD (GreaterDevice vn eff) = OList 
       [ OString $ "Greater Enchanted Device (opened with " ++ show vn ++ "p vis)"
       , OList $ map printMD eff ]
   printMD (Talisman vn eff) = OList 
       [ OString $ "Talisman (opened with " ++ show vn ++ "p vis)"
       , OList $ map printMD eff ]
   printMD (ChargedItem vn eff) = OList 
       [ OString $ "Charged Item (" ++ show vn ++ "charges)"
       , OList [ printMD eff ] ]
   printMD MundaneItem = OString "Mundane Item" 

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

instance Markdown Possession  where
   printMD = defaultMD

instance Markdown Library where
   printMD = defaultMD

-- * Advancements

instance Markdown CovAdvancement where
   printMD = defaultMD 

-- * Covenant Markdown

instance Markdown Lab where
   printMD = defaultMD

-- * Convenience Functions

storyOList :: StoryObject a => a -> [ OList ]
storyOList ob = 
       [ OString $ name ob
       , OList  $ map italicOString $ narrative ob
       , OList  $ map OString $ comment ob 
       ]
