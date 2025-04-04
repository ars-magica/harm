-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.KeyPair
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Generic type for Key/Value pairs.
--
-- The `KeyPairList` type is created to represent arbitrary objects parsed
-- from JSON.  It is not necessarily completely generic, but supports a
-- generic list of label/value pairs, where the value may be a String or an
-- Int.  This supports custom fields in character sheets.
--
-----------------------------------------------------------------------------
module ArM.Types.KeyPair ( FieldValue(..)
                              , KeyPair(..)
                              , KeyPairList(..)
                              ) where

import Data.Aeson
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T


-- | A `KeyPair` is one label/value pair.
data KeyPair = KeyPair { key :: String, value :: FieldValue }
       deriving (Eq)

-- | A `KeyPairList` is a list of label/value pairs.
data KeyPairList = KeyPairList [ KeyPair ]
       deriving (Eq)

-- | A `FieldValue` is the type of the value in a `KeyPair`,
-- supporting Text or Int, or a catch-all JSON value.
-- The catch-all `Value` case prevents some parsing errors 
-- from JSON.
data FieldValue = TextValue T.Text
                | IntValue Int
                | ObjectValue Value
       deriving (Eq)

instance FromJSON KeyPairList  where
  parseJSON = withObject "KeyPairList" $ \obj ->
    return $ KeyPairList 
           $ map ( \ (k,y) -> KeyPair (toString k) (pValue y) )
           $ KM.toList obj
instance ToJSON KeyPairList where 
    toJSON (KeyPairList t) = object $ map pairToJSON t

-- | Auxiliary for `FromJSON`
pValue :: Value -> FieldValue
pValue (Number x) = IntValue $ round x
pValue (String x) = TextValue x
pValue (x) = ObjectValue x

-- | Auxiliary for `ToJSON`
pairToJSON :: KeyPair -> (Key,Value)
pairToJSON (KeyPair a (IntValue b)) = ((fromString a), (toJSON b))
pairToJSON (KeyPair a (TextValue b)) = ((fromString a), (toJSON b))
pairToJSON (KeyPair a (ObjectValue b)) = ((fromString a), (b))

-- = Show Instances
instance Show FieldValue where
   show (IntValue x) = show x
   show (TextValue x) = T.unpack x
   show x = show x
instance Show KeyPair where
   show (KeyPair x  y) = x ++ ":\t" ++ show y ++ "\n"
instance Show KeyPairList where
   show (KeyPairList xs) = ( foldl (++) "" $ map show xs )
