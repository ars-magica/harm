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
import Data.HList
import Data.Maybe

import ArM.Debug.Trace

-- | Render a possession in Markdown.
-- This should be exposed as `printMD` from the Markdown class.
--
-- Different display templates are used for different kinds of
-- possessions.
--
-- Composite objects require a full and verbose format, with the
-- name of the possession as header.  Non-composite objects will
-- ignore the name of the 'Possession' object.
--
-- Lab texts have a simplified display to show only the spell in the
-- case of a singleton.
printPossessionMD :: Possession -> OList 
printPossessionMD ob 
    | isComposite ob = OList [ OString $ pName ob, pMD ob ]
    | isLabText ob = simpleLabTextMD ob
    | otherwise = pMD ob 

-- | The name of a possession as displayed in Markdown 
pName :: Possession -> String
pName ob = name ob ++ cnt
       where cnt | count ob == 1 = ""
                 | otherwise = " (" ++ show (count ob) ++ ")"

{-
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
               ms | "" /= (trim $ bookTitle book) =  bs:ms'
                  | otherwise =  ms'
-}
-- | Render a book in Markdown.
-- This should be exposed as `printMD` from the Markdown class.
printBookH :: Book -> HList
printBookH book = HList (name book) (map (\x->HList x []) lns) 
   where
      lns = filter (/="") $ statline:keyline:cnt:lng:ans
      keyline = "**Key** " ++ bookID book
      statline | "" /= (trim $ bookTitle book) =  ""
               | otherwise = showStrList $ map show (bookStats book) 
      lng = trim $ fromMaybe "" $ bookLanguage book
      ans = map trim $ bookAnnotation book
      cnt | bookCount book == 1 = ""
          | otherwise = show (bookCount book) ++ " copies"
                
-- | List of functions to make Markdown output.
-- Each function in the list provides output for one kind of Possession.
pMDlist :: [ Possession -> OList ]
pMDlist = [ hMD . bookH
          , hMD . labtextH
          , hMD . weaponH
          , hMD . armourH
          , hMD . visH
          , hMD . acH ]

hMD :: Maybe HList -> OList
hMD = fromMaybe (OList []) . fmap fromHList 
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

pHgen :: Possession -> [Possession -> Maybe HList] -> HList
pHgen ob = HList (name ob) . filterNothing . map ($ ob) 



labtextH :: Possession -> Maybe HList
labtextH = f . labTexts
   where f [] = Nothing
         f ls = Just $ HList "Lab Texts" ( map textH ls )

simpleLabTextMD :: Possession -> OList
simpleLabTextMD = fromMaybe (OList []) . fmap fromHList . simpleLabTextH

simpleLabTextH :: Possession -> Maybe HList
simpleLabTextH ob = simpleLabTextH' ob (labTexts ob)

simpleLabTextH' :: Possession -> [LabText] -> Maybe HList
simpleLabTextH' _ [] =  Nothing
simpleLabTextH' _ [x] = Just $ textH x
simpleLabTextH' ob xs = Just $ HList nm ( map textH xs )
         where nm | "" == name ob = "Grimoire"
                  | otherwise = name ob

weaponH :: Possession -> Maybe HList 
weaponH ob | isArmour ob = Just $ ttrace $ toHList ("Weapon Stats":(a1++a2))
           | otherwise = trace "No Weapon" Nothing
           where a1 = weapon ob
                 a2 = map show $ weaponStats ob

armourH :: Possession -> Maybe HList 
armourH ob | isArmour ob = Just $ ttrace $ toHList ("Armour Stats":(a1++a2))
           | otherwise = trace "No Armour" Nothing
           where a1 = armour ob
                 a2 = map show $ armourStats ob

visH :: Possession -> Maybe HList
visH ob | isNothing (itemArt ob) = Nothing
        | otherwise = Just $ HList ( s ++ " vis: " ++ show p ++ " pawns" ) []
         where s = fromJust $ itemArt ob
               p = itemCount ob

-- | Render arcane connection.
acH :: Possession -> Maybe HList
acH = fmap (\ s -> HList ( "Arcane Connection to " ++ s ) [] ) . acTo

bookH :: Possession -> Maybe HList
bookH ob =  (f . bookTexts) ob
      where f [] =  Nothing
            f [x] = Just $ printBookH x
            f xs =  Just $ HList "Antology of" $ map printBookH xs
