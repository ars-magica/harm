{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Calendar
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Narrative time, incl. SeasonTime type
--
-----------------------------------------------------------------------------
module ArM.Types.Calendar ( SeasonTime(..)
                          , Season(..)
                          , parseSeasonTime
                          , isWinter
                          , (>*)
                          ) where

import Data.Text.Lazy                            ( fromStrict, unpack )
import Data.List.Split
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

-- |
-- == Hibernian Calendar

-- | This comparison is used to check if a character is past the age limit where
-- aging rolls are required.
-- ```
-- (*>) = (>=)
-- ```
(>*) :: Ord a => a -> a -> Bool
(>*) = (>)

-- | Season of the year.
-- ```
-- data Season = Spring | Summer | Autumn | Winter | NoSeason
-- ```
data Season = Winter | Spring | Summer | Autumn  | NoSeason
     deriving (Show,Ord,Eq,Read,Generic)


-- |
-- == Generic definitions 

-- | A `SeasonTime` is a point in the narrative time, or `NoTime` for undefined.
data SeasonTime = SeasonTime Season Int | GameStart | NoTime deriving (Eq,Generic)

instance ToJSON SeasonTime where
   toJSON = toJSON . show
-- instance FromJSON SeasonTime 
instance ToJSON Season


instance FromJSON SeasonTime where

    parseJSON (Number n) = pure $ SeasonTime NoSeason $ round n
    parseJSON (String t) = pure $ parseST (unpack (fromStrict t))
    parseJSON _ = mzero

-- | Is the Season Winter?
isWinter :: SeasonTime -> Bool
isWinter (SeasonTime Winter _) = True
isWinter _ = False

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

instance Ord SeasonTime where
    (<=) NoTime _ = False
    (<=) _ NoTime = True
    (<=) GameStart _ = True
    (<=) _ GameStart = False
    (<=) (SeasonTime s1 y1) (SeasonTime s2 y2) 
        | y1 == y2 = s1 <= s2
        | otherwise = y1 <= y2

