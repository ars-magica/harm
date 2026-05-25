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

ab :: Ability
ab = Ability "Cattle Herd" Nothing 0 5 0 1 0
tr :: Trait
tr = AbilityTrait ab
ab2 :: Ability
ab2 = Ability "Single Weapon" Nothing 0 5 0 1 0
sp1 :: Staff
sp1 = Specialist [ tr ]
sp2 :: Staff
sp2 = CovenGrog [ ab2 ]

-- | The program will read the given saga file and constituent files and
-- generate all the character and covenant sheets requested by the saga file.
main :: IO ()
main = do 
     putStrLn "Starting: yamltest ..."
     putStrLn ""
     printB $ encode sp1
     putStrLn ""
     printB $ encode ab
     putStrLn ""
     printB $ encode sp2
     putStrLn ""

