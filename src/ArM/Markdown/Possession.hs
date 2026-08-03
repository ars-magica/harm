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
import ArM.Markdown.HList
import ArM.Story
import ArM.Trait
import ArM.Helper 
import Data.List
import Data.HList
import Data.Maybe
import Control.Monad

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
printPossessionH :: Possession -> HList 
printPossessionH ob 
    | isComposite ob = pH ob
    | isJust (itemName ob) = pH ob
    | isBook ob = hfm "Empty book" $ addGen bookH ob
    | isLabText ob = hfm "Empty lab text" $ simpleLabTextH ob
    | isAC ob = hfm "Bogus arcane connection" $ acHsimple ob
    | isVis ob = hfm "Bogus vis" $ visHsimple ob
    | isVisSrc ob = hfm "Bogus vis" $ visrcHsimple ob
    | isArmour ob = hfm "Bogus armour" $ armourHsimple ob
    | isWeapon ob = hfm "Bogus weapon" $ weaponHsimple ob
    | silver ob /= 0 = hfm "Bogus silver" $ addGen silverH ob
    | silverYield ob /= 0 = hfm "Bogus income" $ addGen incomeH ob
    | isStaff ob = hfm "Bogus staff" $ addGen staffH ob
    | otherwise = pH ob 
     -- hfm :: String -> Maybe HList -> HList
  where hfm s = fromMaybe (HList s [])

-- | Render a list of possessions as a HList, returning Nothing
-- if it is empty.
printPossessionsH :: String -> [Possession] -> Maybe HList 
printPossessionsH _ [] = Nothing
printPossessionsH s ps = Just $ HList s $ map printPossessionH $ sort ps 

-- | Render a book in Markdown.
-- This should be exposed as `printMD` from the Markdown class.
printBookH :: Book -> HList
printBookH book = HList (name book) (map (\x->HList x []) lns) 
   where
      lns = filter (/="") $ statline:keyline:lng:ans
      keyline = "**Key** " ++ bookID book
      statline | "" == (trim $ bookTitle book) =  ""
               | otherwise = showStrList $ map show (bookStats book) 
      lng = trim $ fromMaybe "" $ bookLanguage book
      ans = map trim $ bookAnnotation book

-- * Render composite items

-- | Complete description of a composite item.
-- This is awkward for most items, particularly because names and
-- titles tend to be duplicated, once for the 'Possession' object 
-- and once for the constituent object, but it is necessary for
-- complex items such as enchanted books, magic swords, as well as
-- antologies.
pH :: Possession -> HList
pH ob = pHgen ob pHlist

-- | The name of a possession as displayed in Markdown 
pName :: Possession -> String
pName ob = name ob ++ cnt
       where cnt | count ob == 1 = ""
                 | otherwise = " (" ++ show (count ob) ++ ")"
                
-- | List of functions to make Markdown output.
-- Each function in the list provides output for one kind of Possession.
-- Count is not included as this is set in the possession header.
pHlist :: [ Possession -> Maybe HList ]
pHlist = [ staffH, bookH, labtextH, weaponH, armourH, enchantedH, silverH, incomeH, visH, visrcH, acH, narrativeH, commentH, dateH ]

-- | Render a composite item using the functions provided.
pHgen :: Possession -> [Possession -> Maybe HList] -> HList
pHgen ob = HList (pName ob) . filterNothing . map ($ ob) 

enchantedH :: Possession -> Maybe HList
enchantedH = printEnchantedH . enchantment

printEnchantedH :: Enchantment -> Maybe HList
printEnchantedH (LesserItem eff) = Just $ effectH eff 
printEnchantedH (GreaterDevice vn eff) = Just $ HList 
       ( "Greater Enchanted Device (opened with " ++ show vn ++ "p vis)" )
       ( map effectH eff )
printEnchantedH (Talisman vn eff ats) = Just $ HList 
       ( "Talisman (opened with " ++ show vn ++ "p vis)" )
       $ hlist ( "**Attunements:** " ++ showStrList ats ):map effectH eff 
printEnchantedH (ChargedItem vn eff) = Just $ HList 
       ( "Charged Item (" ++ show vn ++ "charges)" ) [ effectH eff ]
printEnchantedH MundaneItem = Nothing

-- * Individual pieces of information

staffH :: Possession -> Maybe HList
staffH = join . fmap staffH' . staff

staffH' :: Staff -> Maybe HList
staffH' Servant = jhlist "Servant"
staffH' Teamster = jhlist "Teamster"
staffH' Labourer = jhlist "Labourer"
staffH' (CovenGrog xs) = jhlist $ "Grog: " ++ staffHab xs
staffH' (Specialist xs) = jhlist $ "Specialist: " ++ staffHab xs


staffHab :: [ Trait ] -> String
staffHab [] = "No traits"
staffHab xs = ( foldr (++) "" $ (map (++", ") $ map showTrait xs) )

showTrait :: Trait -> String
showTrait (AbilityTrait ab) = abilityName ab ++ " " ++ (show $ abilityScore ab)
showTrait (CharacteristicTrait ab) = characteristicName ab ++ " " 
                                   ++ (show $ charScore ab)
showTrait t = show t

-- | Render books in a possession.
bookH :: Possession -> Maybe HList
bookH ob =  (f . bookTexts) ob
      where f [] =  Nothing
            f [x] = Just $ printBookH x
            f xs =  Just $ HList "Antology of" $ map printBookH xs

bookHsimple :: Possession -> Maybe HList
bookHsimple ob = fmap (appendToHList xtra) $ bookH ob
   where xtra = genEntries ob

-- | Render lab texts in a possession.
labtextH :: Possession -> Maybe HList
labtextH = f . labTexts
   where f [] = Nothing
         f ls = Just $ HList "Lab Texts" ( map textH ls )


-- | Render weapon data.
weaponH :: Possession -> Maybe HList 
weaponH ob | length a2 > 0 = Just $ toHList ("Weapon Stats":a1++a2)
           | isWeapon ob = Just $ HList ("Weapon Stats: " ++ showStrList a1) []
           | otherwise = Nothing
           where a1 = weapon ob
                 a2 = map show $ weaponStats ob

-- | Render weapon data.
weaponHsimple :: Possession -> Maybe HList 
weaponHsimple ob 
    | isJust (itemName ob) = Just $ pH ob
    | length a2 > 0 = Just $ pH ob
    | length a > 1  = Just $ pH ob
    | length a == 1 = Just $ pHsimple (pName ob) ob genList
    | otherwise = Nothing
           where a1 = weapon ob
                 a2 = map show $ weaponStats ob
                 a = a1++a2

-- | Render armour data.
armourHsimple :: Possession -> Maybe HList 
armourHsimple ob 
    | isJust (itemName ob) = Just $ pH ob
    | length a2 > 0 = Just $ pH ob
    | length a > 1  = Just $ pH ob
    | length a == 1 = Just $ pHsimple (pName ob) ob genList
    | otherwise = Nothing
           where a1 = armour ob
                 a2 = map show $ armourStats ob
                 a = a1++a2
-- | Render armour data.
armourH :: Possession -> Maybe HList 
armourH ob | isArmour ob = Just $ toHList ("Armour Stats":a)
           | otherwise = Nothing
           where a1 = armour ob
                 a2 = map show $ armourStats ob
                 a = a1++a2

-- | Render raw vis.
visH :: Possession -> Maybe HList
visH ob | pawns ob == 0 = Nothing
        | otherwise = Just $ hlist ( s ++ " vis: " ++ show p ++ " pawns" )
         where s = fromJust $ itemArt ob
               p = pawns ob

visrcH :: Possession -> Maybe HList
visrcH ob | isVisSrc ob = Just $ hlist (visrc ob) 
          | otherwise = Nothing

vistimeH :: Possession -> Maybe HList
vistimeH = fmap (hlist . ("(Harveste in " ++) . show) . visTime

-- | One line description of a vis source.
visrc :: Possession -> String
visrc ob = "Vis source: " ++ show p ++ " pawns " ++ s ++ " per year" ++ h
         where s = fromMaybe "unknown art" $ itemArt ob
               p = visYield ob
               h = f $ visTime ob
               f Nothing = ""
               f (Just x) = " (harvest in " ++ show x ++ ")"

-- | Render arcane connection data.
acH :: Possession -> Maybe HList
acH = fmap (\ s -> HList ( "Arcane Connection to " ++ s ) [] ) . acTo


-- | Render number of pieces.
countH :: Possession -> Maybe HList
countH = f . count
    where f 1 = Nothing
          f x = Just $ HList (show x ++ " pieces") []

-- | Render the creation date
dateH :: Possession -> Maybe HList
dateH = f . itemDate
    where f NoTime = Nothing
          f x = Just $ HList (show x) []

-- * Simple Items

-- | List of functions to render common elements of possessions
genList :: [ Possession -> Maybe HList ]
genList = [ narrativeH, commentH, dateH ]

-- | List of functions to render common elements of possessions with count
genList' :: [ Possession -> Maybe HList ]
genList' = [ narrativeH, commentH, countH, dateH ]

-- | Render a possession that is only lab texts
simpleLabTextH :: Possession -> Maybe HList
simpleLabTextH ob = fmap (appendToHList xtra) $ simpleLabTextH' ob (labTexts ob)
   where xtra = genEntries ob

-- | Auxiliary for `simpleLabTextH`.
simpleLabTextH' :: Possession -> [LabText] -> Maybe HList
simpleLabTextH' _ [] =  Nothing
simpleLabTextH' _ [x] = Just $ textH x
simpleLabTextH' ob xs = Just $ HList nm ( map textH xs )
         where nm | "" == name ob = "Grimoire"
                  | otherwise = name ob

-- | Generic function to rander simple (non-composite) possessions 
pHsimple :: String -> Possession -> [Possession -> Maybe HList] -> HList
pHsimple s ob = HList s . filterNothing . map ($ ob) 

-- | Render a possesion that s just an arcane connection.
acHsimple :: Possession -> Maybe HList
acHsimple ob = fmap f $ acTo ob
   where f target = pHsimple target ob genList'

-- | Render a possesion that is just raw vis
visHsimple :: Possession -> Maybe HList
visHsimple ob 
  | pawns ob == 0 = Nothing
  | otherwise = Just $ pHsimple v ob genList'
      where v = s ++ " vis: " ++ show p ++ " pawns" 
            s = fromJust $ itemArt ob
            p = pawns ob

visrcHsimple :: Possession -> Maybe HList
visrcHsimple ob | isVisSrc ob = Just $ pHsimple ( visrc ob ) ob genList'
                | otherwise = Nothing

-- | Render the general entries for Possession display.
-- This is used to append to book and lab text displays.
genEntries :: Possession -> [ HList ]
genEntries ob = filterNothing $ map ($ ob) genList'

addGen :: ( Possession -> Maybe HList ) -> Possession -> Maybe HList
addGen f ob = fmap (appendToHList ls) $ f ob
     where ls = genEntries ob

-- | Income
--
-- | Render silver vis.
silverH :: Possession -> Maybe HList
silverH ob | silver ob == 0 = Nothing
           | otherwise = Just $ hlist ( show p ++ " mythic pounds" )
         where p = silver ob

incomeH :: Possession -> Maybe HList
incomeH ob | silverYield ob /= 0 = Just $ hlist (incomeS ob) 
          | otherwise = Nothing

-- | One line description of a vis source.
incomeS :: Possession -> String
incomeS ob = "Income source: " ++ show p ++ " mythic pounds per year" 
         where p = silverYield ob

