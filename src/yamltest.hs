-----------------------------------------------------------------------------
-- |
-- Module      :  yamltest
-- Copyright   :  (c) 2026: Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Quick test of YAML syntax
--
-----------------------------------------------------------------------------

module Main (main) where

import System.IO (stdout)

import ArM.Trait
import Data.Yaml
import qualified Data.ByteString as B

printB :: B.ByteString -> IO ()
printB = B.hPut stdout 

s :: SpellRecord 
s = defaultSpellRecord { spellRecordName = "Invisible Hand"
                , spellRecordTeFo = "ReTe"
                , lvl = Just 5
                , technique = "Rego"
                , techniqueReq = []
                , form = "Terram"
                , formReq = []
                , spellRange = "Voice"
                , spellTarget = "Ind"
                , specialSpell = []
                , spellDescription = ""
                , design = ""
                , spellComment = "Example"
                , cite = "core, I think"
                }

d :: LabText
d = SpellText s

-- | The program will read the given saga file and constituent files and
-- generate all the character and covenant sheets requested by the saga file.
main :: IO ()
main = do 
     putStrLn "Starting: yamltest ..."
     putStrLn ""
     printB $ encode s
     putStrLn ""
     printB $ encode d
     putStrLn ""



