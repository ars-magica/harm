{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.HarmObject
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Classes and keys to provide generic interfaces to types.
--
-- The purpose of this module is to provide a consistent interface and 
-- structure to different parts of the data model.  This includes
-- + The `HarmKey` type used to index arbitrary hArM objects.
--     + Presently, this does not include traits, which has its own
--       `TraitKey`
-- + Classes to provide common APIs
--
-----------------------------------------------------------------------------
module ArM.Types.HarmObject ( HarmKey(..)
                            , HarmObject(..)
                            , KeyObject(..)
                            , StoryObject(..)
                            , Countable(..)
                            , compareKey
                            ) where

import Data.Aeson
import Data.List
import GHC.Generics
import ArM.Debug.Trace
import ArM.Types.Calendar

-- | A unique identifier for objects.
-- It is made quite generic to support a class `KeyObject` of keyed objects
-- without enabling multi-parameter classes.
data HarmKey = BookKey String 
           | CharacterKey String
           | CovenantKey String
           | LabKey String
           | NoObject
           deriving ( Eq, Ord, Generic )

instance ToJSON HarmKey
instance FromJSON HarmKey

instance Show HarmKey where
       show (BookKey x ) = "Book: " ++ x
       show (CharacterKey x) = "Character: " ++ x
       show (CovenantKey x) = "Covenant: " ++ x
       show (LabKey x) = "Lab: " ++ x
       show NoObject = "No Such Object"


-- | Class of types that can be indexed by `HarmKey` objects.
class KeyObject h where
   -- | Return the unique key of the object
   harmKey :: h -> HarmKey
   -- | Find an object by key in a list.
   harmFind :: HarmKey -> [h] -> Maybe h
   harmFind k = find ( (==k) . harmKey )
   -- | Find an object by key in a list.
   harmFilter :: HarmKey -> [h] -> [h]
   harmFilter k = filter ( (==k) . harmKey )
   -- | Find a list of objects by key in a sorted list.
   harmLookup :: [HarmKey] -> [h] -> [h]
   harmLookup (x:xs) (y:ys) 
       | x < harmKey y = trace ("Object not found: "++show x) $ harmLookup xs (y:ys)
       | x > harmKey y = harmLookup (x:xs) ys
       | otherwise = y:harmLookup xs ys
   harmLookup (x:_)  _ = trace ("Object not found: "++show x) []
   harmLookup _ _ = []

   -- | Sorty objects by `HarmKey`
   sortOnKey :: [h] -> [h]
   sortOnKey = sortOn harmKey

-- | Compare to objects by their `HarmKey`.
compareKey :: (KeyObject a,KeyObject b) => a -> b -> Ordering
compareKey x y = compare (harmKey x) (harmKey y)

-- |
-- The `HarmObject` class establishes a common interface for `Covenant` and
-- `Character`.
class (Timed h, StoryObject h) => HarmObject h where
    -- | String identifying the object and its state
    stateName :: h -> String
    stateName x = name x ++ " (" ++ show (season x) ++ ")"


    -- | Is the character state still at Game Start?
    isGameStart :: h -> Bool
    isGameStart = (==GameStart) . season

-- | Class for countable objects.
class Countable c where
   count :: c -> Int
   addCount :: c -> Int -> c

-- | Common interface for objects that have a narrative aspect.
class StoryObject ob where
   -- | The name could be the title of the story or other unique identifier.
   name :: ob -> String
   -- | Description focusing on a narrative feel.
   narrative :: ob -> [ String ]
   narrative _ = []
   -- | Remarks that do not fit in a narrative style, e.g. mechanics.
   comment :: ob -> [ String ]
   comment _ = []
