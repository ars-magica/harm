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
-- 1.  Composite objects require a full and verbose format, with the
-- name of the possession as header.  
-- 2.  Possession objects with an 'itemName' will also use the full
-- format.
--
-- Non-composite objects will often infer the header, instead of using
-- the possesion `name`.
--
-- 3. Lab texts have a dedicated function to make sure that the typical
-- case of a single scroll with one effect is handled without fuss.
--
printPossessionH :: Possession -> HList 
printPossessionH ob 
    | isComposite ob = pH ob
    | isJust (itemName ob) = pH ob
    | isBook ob = hfm "Empty book" $ bookH ob
    | isLabText ob = hfm "Empty lab text" $ simpleLabTextH ob
    | isAC ob = hfm "Bogus arcane connection" $ acHsimple ob
    | isVis ob = hfm "Bogus vis" $ visHsimple ob
    | otherwise = pH ob 

hfm :: String -> Maybe HList -> HList
hfm s = fromMaybe (HList s [])

-- | The name of a possession as displayed in Markdown 
pName :: Possession -> String
pName ob = name ob ++ cnt
       where cnt | count ob == 1 = ""
                 | otherwise = " (" ++ show (count ob) ++ ")"

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
pHlist :: [ Possession -> Maybe HList ]
pHlist = bookH:labtextH:weaponH:armourH:visH:acH:genList

-- | Convert HList output to OList outputa.
hMD :: Maybe HList -> OList
hMD = fromMaybe (OList []) . fmap fromHList 

-- | Complete description of a composite item.
-- This is awkward for most items, particularly because names and
-- titles tend to be duplicated, once for the 'Possession' object 
-- and once for the constituent object, but it is necessary for
-- complex items such as enchanted books, magic swords, as well as
-- antologies.
pH :: Possession -> HList
pH ob = pHgen ob pHlist

-- | Render a composite item using the functions provided.
pHgen :: Possession -> [Possession -> Maybe HList] -> HList
pHgen ob = HList (pName ob) . filterNothing . map ($ ob) 

labtextH :: Possession -> Maybe HList
labtextH = f . labTexts
   where f [] = Nothing
         f ls = Just $ HList "Lab Texts" ( map textH ls )

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

narrativeH :: Possession -> Maybe HList
narrativeH = effectMP "Background" . map italic . narrative

commentH :: Possession -> Maybe HList
commentH = effectMP "Comment" . comment

countH :: Possession -> Maybe HList
countH = f . count
    where f 1 = Nothing
          f x = Just $ HList (show x ++ " pieces") []
dateH :: Possession -> Maybe HList
dateH = f . itemDate
    where f NoTime = Nothing
          f x = Just $ HList (show x) []

genList :: [ Possession -> Maybe HList ]
genList = [ narrativeH, commentH, countH, dateH ]

pHsimple :: String -> Possession -> [Possession -> Maybe HList] -> HList
pHsimple s ob = HList s . filterNothing . map ($ ob) 

-- | Render arcane connection.
acHsimple :: Possession -> Maybe HList
acHsimple ob = fmap f $ acTo ob
   where f target = pHsimple target ob genList

visHsimple :: Possession -> Maybe HList
visHsimple ob 
  | isNothing (itemArt ob) = Nothing
  | otherwise = Just $ pHsimple v ob genList
      where v = s ++ " vis: " ++ show p ++ " pawns" 
            s = fromJust $ itemArt ob
            p = itemCount ob

bookH :: Possession -> Maybe HList
bookH ob =  (f . bookTexts) ob
      where f [] =  Nothing
            f [x] = Just $ printBookH x
            f xs =  Just $ HList "Antology of" $ map printBookH xs
