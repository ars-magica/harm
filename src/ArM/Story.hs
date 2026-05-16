{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Story
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to handle the narrative and temporal aspects of stories.
-- 
-- This module includes the calendar as well as the `StoryObject` class
-- which supports narrative text.  It also includes the `HarmObject` class
-- for more generic story objects.
--
-----------------------------------------------------------------------------
module ArM.Story ( Story(..)
                 , StoryObject(..)
                 , SeasonTime(..)
                 , Season(..)
                 , parseSeasonTime
                 , seasonNext
                 , seasonPrev
                 , (>*)
                 , Timed(..)
                 , showKey
                 , gameSeason
                 , HarmKey(..)
                 , HarmObject(..)
                 , KeyObject(..)
                 , Countable(..)
                 , compareKey
                 ) where

import ArM.Story.Calendar
import ArM.Story.HarmObject
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
