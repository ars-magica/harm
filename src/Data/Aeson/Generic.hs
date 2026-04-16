----------------------------------------------------------------------------- -- |
-- Module      :  Data.Aeson.Generic
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Generic file reader for JSON and YAML.
--
-----------------------------------------------------------------------------
module Data.Aeson.Generic (readObject) where

import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import System.FilePath (splitExtension)

-- | Read and parse an object from a JSON or YAML file.
-- The filename has to end in .yaml or .yml for YAML and .json for JSON.
readObject :: A.FromJSON t => String -> IO (Maybe t)
readObject = readObject' . typedFile

-- | Read and parse an object from a JSON or YAML file.
-- This is a helper for `readObject`
readObject' :: A.FromJSON t => TypedFileName -> IO (Maybe t)
readObject' (YAML fn) = B.readFile fn >>= return . Y.decodeThrow
readObject' (JSON fn) = LB.readFile fn >>= return . A.decode
readObject' (UnknownType _) = error "Unknown file extension"

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

