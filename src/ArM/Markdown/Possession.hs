{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Possession
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  MarkDown output for Possession.
--
-- This is an internal module.
--
-----------------------------------------------------------------------------
module ArM.Markdown.Possession where

import ArM.Markdown.Spell
import ArM.Story
import ArM.Trait
import ArM.Helper 
import Data.OList
import Data.Maybe

-- | Render a possession in Markdown
-- This should be exposed as `printMD` from the Markdown class.
printPossessionMD :: Possession -> OList 
printPossessionMD ob 
    | isComposite ob = OList [ OString $ pName ob, pMD ob ]
    | otherwise = pMD ob 

-- | The name of a possession as displayed in Markdown 
pName :: Possession -> String
pName ob = name ob ++ cnt
       where cnt | count ob == 1 = ""
                 | otherwise = " (" ++ show (count ob) ++ ")"

-- | Render a book in Markdown
printBookMD :: Book -> OList
printBookMD book = OList [ OString $ name book, OList ms ]
         where ans = map ( f . trim ) $ bookAnnotation book
               f "" = OList []
               f s = OString s
               lng = trim $ fromMaybe "" $ bookLanguage book
               lns | lng == "" = OList []
                   | otherwise = OString $ "in " ++ lng
               cnt | bookCount book == 1 = OList []
                   | otherwise = OString $ show (bookCount book) ++ " copies"
               ms' = OString ("**Key** " ++ bookID book):cnt:lns:ans
               bs = OString $ showStrList $ map show (bookStats book) 
               ms | "" /= (trim $ bookTitle book) = bs:ms'
                  | otherwise = ms'
               -- k | bookID book /= "" = " [" ++ bookID book ++ "]"
               --   | otherwise = ""
                

-- | List of functions to make Markdown output.
-- Each function in the list provides output for one kind of Possession.
pMDlist :: [ Possession -> OList ]
pMDlist = [ bookMD, labtextMD, weaponMD, armourMD, visMD, acMD ]

-- | Complete description of a composite item.
-- This is awkward for most items, particularly because names and
-- titles tend to be duplicated, once for the 'Possession' object 
-- and once for the constituent object, but it is necessary for
-- complex items such as enchanted books, magic swords, as well as
-- antologies.
pMD :: Possession -> OList
pMD ob = pMDgen ob pMDlist

pMDgen :: Possession -> [Possession -> OList] -> OList
pMDgen ob = foldOList . OList . filter (not . isEmptyOList) . map ($ ob) 

labtextMD :: Possession -> OList
labtextMD ob | labTexts  ob == []  =  OList []
             | otherwise = OList [ OString "Lab Texts" 
                             , OList $ map f (labTexts ob) ]
         where f (SpellText x) = OList
                         [ OString $ spellRecordName x
                         , coreSpellRecordMD (Just x) ]
               f (Device x) = printEffectMD x

weaponMD :: Possession -> OList 
weaponMD ob | isWeapon ob = OList
                  [ OString "Weapon Stats"
                  , OList $ map OString $ weapon ob
                  , OList $ map ( OString . show ) $ weaponStats ob
                  ] 
            | otherwise = OList []
armourMD :: Possession -> OList 
armourMD ob | isArmour ob = OList
                  [ OString "Armour Stats"
                  , OList $ map OString $ armour ob
                  , OList $ map ( OString . show ) $ armourStats ob
                  ] 
            | otherwise = OList []


visMD :: Possession -> OList
visMD ob | isNothing (itemArt ob) = OList []
         | otherwise = OString ( s ++ " vis: " ++ show p ++ " pawns" )
         where s = fromJust $ itemArt ob
               p = itemCount ob

acMD :: Possession -> OList
acMD = f . acTo
    where f Nothing = OList []
          f (Just s) = OString ( "Arcane Connection to " ++ s )

bookMD :: Possession -> OList
bookMD =  f . bookTexts
      where f [] =  OList []
            f [x] = printBookMD x
            f xs =  OList [ OString "Antology of"
                         , OList $ map printBookMD xs
                         ]
