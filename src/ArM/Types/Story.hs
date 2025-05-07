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
                       , StoryObject(..)
                       ) where

import ArM.Types.Calendar
import ArM.Types.HarmObject
import Data.Aeson 
import Data.Aeson.Extra
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
        <*> v `parseCollapsedList` "narrative" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "SQ" 


instance StoryObject Story where
   name = storyTitle
   setName n x = x { storyTitle = n }
   narrative = storyNarrative
   comment = storyComment
   addNarrative s x = x { storyNarrative = s:storyNarrative x }
   addComment s x = x { storyComment = s:storyComment x }
