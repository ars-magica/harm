{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Story
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The Story type to add narrative and SQ to covenant advancement.
--
-----------------------------------------------------------------------------
module ArM.Types.Story ( Story(..)
                          ) where

import ArM.Types.Calendar
import Data.Aeson 
import GHC.Generics

data Story = Story 
         { storySeason :: SeasonTime
         , storyTitle :: String
         , storyNarrative :: [ String ]
         , storyComment :: [ String ]
         , storySQ :: Maybe Int
       }  deriving (Eq,Generic,Show)

instance Timed Story where
    season = storySeason

instance ToJSON Story 
instance FromJSON Story where
    parseJSON = withObject "Story" $ \v -> Story
        <$> v .:? "season" .!= NoTime
        <*> v .:? "title" .!= ""
        <*> v .:? "narrative" .!= []
        <*> v .:? "comment" .!= []
        <*> v .:? "SQ" 

