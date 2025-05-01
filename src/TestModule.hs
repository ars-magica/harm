{-# LANGUAGE OverloadedStrings #-}

module TestModule where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import Control.Monad

type Val = Int

data Request = Req1 { id :: String, properties :: Val }
             | Req2 { id :: String, properties :: Val }

instance FromJSON Request where
  parseJSON (Object v) =
    case H.lookup "req1" v of
      Just (Object h) -> Req1 <$> h .: "id" <*> h .: "properties"
      Nothing -> 
        case H.lookup "req2" v of
          Just (Object h) -> Req2 <$> h .: "id" <*> h .: "properies"
          Nothing -> mzero
