----------------------------------------------------------------------------- -- |
-- Module      :  Data.Aeson.Generic
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  
--    Generic file reader for JSON and YAML.
--
-- This module exploits the fact that the Yaml module reuses the
-- ToJSON/FromJSON instances from Aeson to process Yaml files.
-- The `readObject` function checks the file extension and uses
-- the decoder from the appropriate library to parse either YAML
-- or JSON.
--
-----------------------------------------------------------------------------
module Data.Aeson.Generic (readObject) where

import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import System.FilePath (splitExtension)

import ArM.Debug.Trace

-- | Read and parse an object from a JSON or YAML file.
-- The filename has to end in .yaml or .yml for YAML and .json for JSON.
readObject :: A.FromJSON t => String -> IO (Maybe t)
readObject fn = trace fn $ readObject' $ typedFile fn

-- | Read and parse an object from a JSON or YAML file.
-- This is a helper for `readObject`
readObject' :: A.FromJSON t => TypedFileName -> IO (Maybe t)
readObject' (YAML fn) = B.readFile fn >>= maybeError fn . Y.decode
readObject' (JSON fn) = LB.readFile fn >>= maybeError fn . A.decode
readObject' (UnknownType fn) = putStrLn ("ERROR [readObject] Unknown file extension: " ++ fn) 
                             >> return Nothing

maybeError :: String -> Maybe t -> IO (Maybe t)
maybeError fn Nothing = putStrLn  ("ERROR [readObject] Failed to read file: " ++ fn) 
                      >> return Nothing
maybeError _ x = return x


-- | Typed filename to distinguish YAML and JSON files. 
data TypedFileName = YAML String | JSON String | UnknownType String

-- | Check file extension
typedFile :: String -> TypedFileName
typedFile fn | getExtension fn == ".yaml" = YAML fn
             | getExtension fn == ".yml" = YAML fn
             | getExtension fn == ".json" = JSON fn
             | otherwise = UnknownType fn


-- | Get the file extension from a filename
getExtension :: String  -- ^ Filename
             -> String  -- ^ File extension
getExtension = snd . splitExtension

