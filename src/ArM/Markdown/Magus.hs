{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Magus
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  The magus part of the character sheet.
--
-----------------------------------------------------------------------------
module ArM.Markdown.Magus where

import ArM.Character 
import ArM.Types.Harm
import ArM.Trait
import ArM.GameRules

import Data.OList
import Data.HList

import ArM.Debug.Trace

-- | Print a table of casting totals for every TeFo combination.
printCastingTotals :: Character -> [String]
printCastingTotals c 
      | Magus /= characterType c = []
      | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                    "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                    lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (castingTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

-- | Print a table of casting totals for every TeFo combination.
printLabTotals :: Character -> [String]
printLabTotals c 
             | Magus /= characterType c = []
             | otherwise = "":"| Lab Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]


-- ** Pretty print arts
-- | Render art scores and vis stocks as a table
artVisH :: Character -> HList
artVisH c | isMagus c = HList "" $ map hlist $ artVisMD' c
        | otherwise = HList "" []


-- | Render art scores and vis stocks as a table
artVisMD :: Character
      -> OList
artVisMD c | isMagus c = toOList $ artVisMD' c
        | otherwise = OList []

-- | Render art scores and vis stocks as a table
artVisMD' :: Character
          -> [ String ]
artVisMD' = ("":) . (h1:) . (h2:) . artVisBody
   where h1 = "| Art  | Score | XP | Vis |" 
         h2 = "| -: | -: | -: | -: |"

-- | Auxiliar for 'artVisMD'' rendering the body of the table.
artVisBody :: Character
           -> [ String ]
artVisBody cs = map artVisLine $ mergeArt as bs
   where as = (map tupleArt . sortTraits . artList ) cs
         bs = sheetVis cs
         tupleArt a = (traitKey a,artName a, artScore a,artExcessXP a)

-- | Merge lists of art traits and vis possessions.
-- This is rather crude to say the least.
mergeArt :: [(TraitKey,String,Int,XPType)] -> [(TraitKey,Int)] 
         -> [(TraitKey,String,Int,XPType,Int)]
mergeArt [] [] = []
mergeArt [] ((y1,y2):ys) = (y1,f y1,0,0,y2):mergeArt [] ys
     where f (ArtKey xn) = xn
           f _ = trace "ERROR: Not an art in mergeArt." ""
mergeArt ((x1,x2,x3,x4):xs) []  = (x1,x2,x3,x4,0):mergeArt xs [] 
mergeArt ((x1,x2,x3,x4):xs) ((y1,y2):ys) 
     | x1 == y1 = (x1,x2,x3,x4,y2):mergeArt xs ys
     | x1 < y1 = (x1,x2,x3,x4,0):mergeArt xs ((y1,y2):ys) 
     | otherwise = (y1,f y1,0,0,y2):mergeArt ((x1,x2,x3,x4):xs) ys
     where f (ArtKey xn) = xn
           f _ = trace "ERROR: Not an art in mergeArt." ""


-- | Auxiliary for 'artVisMD', rendering a single line in the table
artVisLine :: (TraitKey,String,Int,XPType,Int) -> String
artVisLine (_,s,i1,i2,i3) = 
        "| " ++ s  ++ " | " ++ show i1 ++ " | " ++ showNum i2 ++ " | " ++ show i3 ++ " |"

-- | Return the sum of levels in the list of spells.
totalLevels :: [Spell] -> Int
totalLevels = sum . map spellLevel

