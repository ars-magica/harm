{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Story.Calendar
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Narrative time, incl. SeasonTime type
--
-----------------------------------------------------------------------------
module ArM.Story.Calendar ( SeasonTime(..)
                          , Season(..)
                          , parseSeasonTime
                          , seasonNext
                          , seasonPrev
                          , (>*)
                          , Timed(..)
                          , showKey
                          , gameSeason
                          ) where

import Data.Text.Lazy                            ( fromStrict, unpack )
import Data.List.Split
import Data.Lists
import Data.Aeson 
import Control.Monad
import GHC.Generics
import Text.Read             (readMaybe)

-- |
-- = Calendar
--
-- The software assumes the Hibernian calendar, with Winter being the first season of the year.
-- Several things may have to change for the standard calendar with Winter as the last season
-- of the year.  We have tried to collect these definitions here.

-- | This comparison is used to check if a character is past the age limit where
-- aging rolls are required.  The definition should be changed for the Hibernian
-- calendar.
-- ```
-- (>*) = (>)
-- ```
(>*) :: Ord a => a -> a -> Bool
(>*) = (>=)

-- | Season of the year.
-- The Hibernian calendar requires a different definition.
-- ```
-- data Season = Winter | Spring | Summer | Autumn  | NoSeason
-- ```
-- If the Hibernian calender is used `seasonNext` and `seasonPrev` have
-- to be changed as well.
data Season = Spring | Summer | Autumn | Winter | NoSeason
     deriving (Show,Ord,Eq,Read,Generic)
instance FromJSON Season
instance ToJSON Season

-- | A `SeasonTime` is a point in the narrative time, either
-- season and year, `GameStart`, or `NoTime` for undefined.
data SeasonTime = SeasonTime Season Int | GameStart | NoTime deriving (Eq,Generic)

-- | Returns the season following the given one.
seasonNext :: SeasonTime -> SeasonTime
seasonNext GameStart = NoTime
seasonNext NoTime = NoTime
seasonNext (SeasonTime Winter y) = SeasonTime Spring (y+1)
seasonNext (SeasonTime Spring y) = SeasonTime Summer y
seasonNext (SeasonTime Summer y) = SeasonTime Autumn y
seasonNext (SeasonTime Autumn y) = SeasonTime Winter (y)
seasonNext (SeasonTime NoSeason y) = SeasonTime NoSeason (y+1)

-- | Returns the season preceeding the given one.
seasonPrev :: SeasonTime -> SeasonTime
seasonPrev GameStart = NoTime
seasonPrev NoTime = NoTime
seasonPrev (SeasonTime Winter y) = SeasonTime Autumn y
seasonPrev (SeasonTime Spring y) = SeasonTime Winter (y-1)
seasonPrev (SeasonTime Summer y) = SeasonTime Spring y
seasonPrev (SeasonTime Autumn y) = SeasonTime Summer y
seasonPrev (SeasonTime NoSeason y) = SeasonTime NoSeason (y-1)


instance ToJSON SeasonTime where
   toJSON = toJSON . show

instance FromJSON SeasonTime where
    parseJSON (Number n) = pure $ SeasonTime NoSeason $ round n
    parseJSON (String t) = pure $ parseST (unpack (fromStrict t))
    parseJSON _ = mzero


-- | Parse SeasonTime from String
parseST :: String -> SeasonTime
parseST  "GameStart" = GameStart
parseST  "Game Start" = GameStart
parseST  "Start" = GameStart
parseST  "Notime" = NoTime
parseST  "NoTime" = NoTime
parseST  "No Time" = NoTime
parseST  "N/A" = NoTime
parseST  s = fy ys
    where xs = splitOn " " s
          ys = map readMaybe xs :: [Maybe Int]
          ss = map readMaybe xs :: [Maybe Season]
          fs [] = NoSeason
          fs (Nothing:rest) = fs rest
          fs (Just r:_) = r
          st = fs ss
          fy [] = NoTime
          fy (Nothing:rest) = fy rest
          fy (Just r:_) = SeasonTime st r
-- | Parse SeasonTime from `Maybe String`
parseSeasonTime :: Maybe String -> SeasonTime
parseSeasonTime Nothing = NoTime
parseSeasonTime (Just s) = parseST s

instance Show SeasonTime where
   show GameStart = "Game Start"
   show (SeasonTime s y) = show s ++ " " ++ show y
   show NoTime =  "No Time"

showKey' :: SeasonTime -> String
showKey' GameStart = "0010_GameStart"
showKey' (SeasonTime s y) = show y ++ "_" ++ show s ++ show y
showKey' NoTime =  "9999_NoTime"
showKey :: Timed a => a  -> String
showKey = showKey' . season

instance Ord SeasonTime where
    (<=) NoTime _ = False
    (<=) _ NoTime = True
    (<=) GameStart _ = True
    (<=) _ GameStart = False
    (<=) (SeasonTime s1 y1) (SeasonTime s2 y2) 
        | y1 == y2 = s1 <= s2
        | otherwise = y1 <= y2

-- |
-- The Timed Class provides a standardised API for objects which has state
-- and a time stamp in narrative time.
class Timed a where
   season :: a -> SeasonTime -- ^ season of last advancement stage
   (<::) :: a -> a -> Bool 
   (<::) x y = season x < season y
   (>::) :: a -> a -> Bool 
   (>::) x y = season x > season y
   compareTimed :: a -> a -> Ordering
   compareTimed x y = compare  (season y) (season x)
   mergeByTime :: [a] -> [a] -> [ a ]
   mergeByTime = mergeBy compareTimed
   mergeTimed :: [ [a] ] -> [ a ]
   mergeTimed = foldl mergeByTime []
   -- | Is the Season Winter?
   isWinter :: a -> Bool
   isWinter = isWinter' . season


-- | The `gameSeason` of an object is the season for which the object is prepared
-- to play.  The `season` function returns the season of last advancement.
gameSeason :: Timed a => a -> SeasonTime
gameSeason = f . season
   where f GameStart = GameStart
         f x = seasonNext x

instance Timed SeasonTime where
   season = id

-- | Is the Season Winter?
isWinter' :: SeasonTime -> Bool
isWinter' (SeasonTime Winter _) = True
isWinter' _ = False
