{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.HarmKey
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  HarmKey type used to index arbitrary hArM objects.
--
-----------------------------------------------------------------------------
module ArM.Types.HarmKey where

import Data.Aeson
import Data.List
import GHC.Generics

-- | A unique identifier for objects.
-- It is made quite generic to support a class `KeyObject` of keyed objects
-- without enabling multi-parameter classes.
data HarmKey = BookKey String String
           | CharacterKey String
           | CovenantKey String
           | LabKey String
           deriving ( Eq, Generic )

instance ToJSON HarmKey
instance FromJSON HarmKey

instance Show HarmKey where
       show (BookKey ti au) = "Book: " ++ ti ++ " by " ++ au
       show (CharacterKey x) = "Character: " ++ x
       show (CovenantKey x) = "Character: " ++ x
       show (LabKey x) = "Lab: " ++ x

class KeyObject h where
   -- | Return the unique key of the object
   key :: h -> HarmKey
   -- | Find an object by key in a list.
   harmFind :: HarmKey -> [h] -> Maybe h
   harmFind k = find ( (==k) . key )
