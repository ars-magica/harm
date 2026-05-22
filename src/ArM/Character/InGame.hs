{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.InGame
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  In-Game advancement of Character.
--
--
-----------------------------------------------------------------------------
module ArM.Character.InGame where

import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Character.Advancement
import ArM.Story
import ArM.Processing
import ArM.Helper
import Data.Maybe

-- | Initialise `Character` object for advancement
initAdvancement :: SeasonTime -> Character -> Character
initAdvancement t c = c { pastAdvancement = x:pastAdvancement c
                        , futureAdvancement = xs 
                        , state = Just $ f (state c) t
                        }
     where (x,xs) = iaHead t st $ futureAdvancement c
           f Nothing y = defaultCS { charTime = y }
           f (Just s) y = s { charTime = y }
           st = fromMaybe defaultCS $ state c

-- | Empty augmented advancement object with the given time stamp
noAdvT :: SeasonTime -> Augmented Advancement
noAdvT t = Adv a defaultAdvancement []
   where a = defaultAdvancement { advSeason = t }

-- | Empty augmented advancement object
noAdv :: Augmented Advancement
noAdv = noAdvT NoTime

-- | Take the head off the future advancement if the time is right.
iaHead :: SeasonTime -> CharacterState 
       -> [Advancement] -> (Augmented Advancement,[Advancement])
iaHead t _ [] = (noAdvT t,[])
iaHead t st (x:xs) | season x == t = (prepareAdvancement st x,xs)
                   | otherwise = (noAdvT t,xs)

-- | Get the current contracted advancement being processed.
chgCurrentAdv :: Character -> Advancement
chgCurrentAdv = contractAdvancement . fromMaybe noAdv . mhead . pastAdvancement
