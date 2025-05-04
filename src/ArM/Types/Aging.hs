{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Aging
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Managing of character aging 
--
--
-----------------------------------------------------------------------------
module ArM.Types.Aging where

-- import ArM.GameRules
-- import ArM.Helper
-- import ArM.Types.TraitKey
-- import ArM.Types.HarmObject
-- import ArM.Debug.Trace

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
import Data.Maybe

-- | The `Age` type is a kind of character `Trait`.
data Age = Age
    { ageYears :: Int             -- ^ character age in years
    , apparentYounger :: Int      -- ^ difference between age and apparent age
    , ageLimit :: Int
    , longevityRitual :: Int      -- ^ Score of longevity ritual (LR), negative number means none
    , agingRollBonus :: Int       -- ^ Bonus to aging rolls (excluding LR)
    , ageComment :: Maybe String  -- ^ freeform comment
    } deriving (Show,Ord,Eq,Generic)
instance ToJSON Age
instance FromJSON Age where
    parseJSON = withObject "Age" $ \v -> Age
        <$> v .:? "age" .!= 0
        <*> v .:? "apparentYounger"  .!= 0
        <*> v .:? "ageLimit"  .!= 35
        <*> v .:? "longevityRitual"  .!= 0
        <*> v .:? "agingRollBonus"  .!= 0
        <*> v `parseCollapsedList` "comment"  

defaultAging :: Aging
defaultAging = Aging
    { addYears       = Nothing
    , deltaYounger   = Nothing
    , agingRollDie   = Nothing
    , agingRoll      = Nothing
    , longevity      = Nothing
    , agingLimit     = Nothing
    , agingBonus     = Nothing
    , agingComment   = Nothing
    }
-- | The `Aging` is a `ProtoTrait` representing changes to the
-- `Age` trait.
data Aging = Aging
    { addYears       :: Maybe Int
    , deltaYounger   :: Maybe Int   
        -- ^ Should be 1 when age changes and apparent age does not, otherwise 0
    , agingRollDie   :: Maybe Int    -- ^ aging roll die result
    , agingRoll      :: Maybe Int    -- ^ aging roll total
    , longevity      :: Maybe Int    -- ^ score of new longevity ritual
    , agingLimit     :: Maybe Int    -- ^ age when aging rolls are required
    , agingBonus     :: Maybe Int    -- ^ Bonus to aging rolls (excluding LR)
    , agingComment   :: Maybe String -- ^ freeform comment
    } deriving (Ord,Eq,Generic)
instance ToJSON Aging
instance FromJSON Aging 

instance Show Aging where
    show x = "Aging " ++ y ++ lr ++ roll ++ lim ++ b ++ fromMaybe "" (agingComment x)
       where y | isNothing (addYears x) = ""
               | otherwise = show yr ++ " years; apparent " 
                    ++ show (yr-del) ++ " years."
             yr = fromJust $ addYears x
             del = fromMaybe 0 $ deltaYounger x
             lr | isNothing (longevity x) = ""
               | otherwise = " LR " ++ show (fromJust $ longevity x) ++ "; "
             lim | isNothing (agingLimit x) = ""
                | otherwise = "(limit " ++ show (fromJust $ agingLimit x) ++ ") "
             b | isNothing (agingBonus x) = ""
                | otherwise = "(bonus " ++ show (fromJust $ agingBonus x) ++ ") "
             roll | isNothing (agingRoll x) = " No roll. "
                | otherwise = "Rolled " ++ show (fromJust $ agingRoll x) ++ " ("
                           ++ show (fromMaybe (-1) $ agingRollDie x) ++ ") "


advanceAge :: Aging -> Age -> Age
advanceAge ag x = updateLR (longevity ag ) 
                     $ updateABonus ( agingBonus ag )
                     $ updateAge ( addYears ag )
                     $ x { apparentYounger = apparentYounger x + del }
          where updateLR Nothing y = y
                updateLR (Just lr) y = y { longevityRitual = lr }
                updateABonus Nothing y = y
                updateABonus (Just b) y = y { agingRollBonus = agingRollBonus y + b }
                updateAge Nothing y = y
                updateAge (Just b) y = y { ageYears = ageYears y + b }
                del = fromMaybe 0 $ deltaYounger ag

