{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.HOutput
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
module ArM.Markdown.HOutput where

import Data.HList
import Data.OList
import ArM.Markdown.Spell
import ArM.Trait
import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Story
import ArM.Helper
import Control.Monad.State.Lazy
import Data.Maybe

-- | Render the narrative comment.
narrativeH :: StoryObject a => a -> Maybe HList
narrativeH = effectMP "Background" . map italic . narrative

-- | Render the comment.
commentH :: StoryObject a => a -> Maybe HList
commentH = effectMP "Comment" . comment

-- | Make a Maybe HLIst from a string
jhlist :: String -> Maybe HList
jhlist = jhlist

class HOutput h where
   -- | Render an object in 'HList' format allowing markdown notation.
   printH :: h -> Maybe HList
   printS :: h -> State Saga (Maybe HList)
   printS = return . printH
   defaultMD :: h -> OList
   defaultMD = fromMaybe (OString "") . fmap fromHList . printH

instance HOutput LabText where
   printH = Just . textH

instance HOutput SpellRecord where
   printH = Just . spellH
instance HOutput MagicEffect where
   printH = Just . effectH
instance HOutput ProtoTrait where
   printH = jhlist . show 
instance HOutput Trait where
   printH (AgeTrait x) = printH  x
   printH x = jhlist $ show x

instance HOutput Age where
   printH c = Just $ HList h lr
      where y = ageYears c
            lrs = longevityRitual c
            lr | lrs < 0 = []
               | otherwise = [ hlist $ " Longevity Ritual: " ++ show lrs ]
            h = "+ **Age:** " ++ show y ++ " years (apparent age " 
                ++ show (y - apparentYounger c)  ++ ")" 

instance HOutput LabBonus where
   printH (LabBonus x "" z) = jhlist $ x ++ " " ++ showBonus z
   printH (LabBonus _ y z) = jhlist $ y ++ " " ++ showBonus z


-- ** Derived instances

instance HOutput a => HOutput (Maybe a) where
   printH Nothing = Nothing
   printH (Just x) = printH x
