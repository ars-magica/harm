-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.DB.CSV
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  The ArMCSV class supports parsing a type from CSV files.
--
-- This is an internal module. The 'ArMCSV' class is exposed from "ArM.DB".
--
-----------------------------------------------------------------------------
module ArM.DB.CSV where

import ArM.Trait

import ArM.Debug.Trace

import qualified Data.Map as M
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec
import Text.Read
import Data.List.Split

class ArMCSV t where
    -- | Parse the cells of one line from the CSV file into a SpellRecord object.
    fromCSVline :: [String] -> t
    defaultObject :: t
    getID :: t -> String

    -- | Create a `Data.Map.Map` of SpellRecord objects.  
    -- The input is the output from `Data.CSV.csvFile`
    getDB :: [[String]] -> M.Map String t
    getDB = M.fromList . map ( \ x -> (getID x,x) ) . map fromCSVline

    -- | Read spells from CSV.  Return Maybe SpellDB.
    readDB :: String -- ^ Filename
              -> IO (Maybe (M.Map String t))
    readDB fn = parseFromFile CSV.csvFile fn >>= return . Just . getDB . g
      where g (Left _) = [[]]
            g (Right x) = x

instance ArMCSV SpellRecord where
   fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:_) =
      defaultObject { spellRecordName = x1 
                , spellRecordTeFo = x2
                , lvl = readMaybe x7
                , technique = x3
                , techniqueReq = filter (/="") $ splitOn ";" x4
                , form = x5
                , formReq = filter (/="") $ splitOn ";" x6
                , spellRange = x8
                , spellDuration = x9
                , spellTarget = x10
                , specialSpell =  filter (/="") $ splitOn ";" x11
                , spellDescription = x12
                , design = x13
                , spellComment = x14
                , cite = x15
                }
   fromCSVline _ = defaultObject
   defaultObject = SpellRecord
                   { spellRecordName = ""
                   , spellRecordTeFo = ""
                   , lvl = Nothing
                   , technique = ""
                   , techniqueReq = []
                   , form = ""
                   , formReq = []
                   , spellRange = "Per"
                   , spellDuration = "Mom"
                   , spellTarget = "Ind"
                   , specialSpell = []
                   , spellDescription = ""
                   , design = ""
                   , spellComment = ""
                   , cite = ""
                   }
   getID = spellRecordName

instance ArMCSV Weapon where
   fromCSVline (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:_) =
      defaultObject { weaponName = x1 
                , weaponAbility = x2
                , weaponInit = read x3
                , atk = readMaybe x4
                , def = readMaybe x5
                , dam = readMaybe x6
                , range = readMaybe x7
                , str = readMaybe x8
                , load = read x9
                , weaponCost = x10
                }
   fromCSVline _ = defaultObject
   defaultObject = Weapon
                { weaponName = "" 
                , weaponAbility = ""
                , weaponInit = 0
                , atk = Nothing
                , def = Nothing
                , dam = Nothing
                , str = Nothing
                , range = Nothing
                , load = 0
                , weaponCost = "N/A"
                }
   getID = weaponName

instance ArMCSV Armour where
   fromCSVline (x1:x2:x3:x4:_) =
      defaultObject { armourName = x1 
                , armourLoad = trace ("x2 = " ++ x2) $ read x2
                , armourProtection = trace ("x3 = " ++ x3) $ read x3
                , armourCost = x4
                }
   fromCSVline _ = defaultObject
   defaultObject = Armour
                { armourName = "" 
                , armourLoad = 0
                , armourProtection = 0
                , armourCost = "N/A"
                }
   getID = armourName

