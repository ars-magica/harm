{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.CharacterSheet
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
--
-- The CharacterSheet type is a façade exposing lists for each kind of trait.
-- The module also includes convenience functions to calculate derived stats
-- such as casting totals.
--
-----------------------------------------------------------------------------
module ArM.Character.CharacterSheet where

import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Trait
import ArM.DB
import ArM.Helper

import Data.Maybe
import Data.List
import Control.Monad

-- import ArM.Debug.Trace


vfList :: Character -> [ VF ]
vfList = filterNothing . map getTrait . traits
abilityList :: Character -> [ Ability ]
abilityList = filterNothing . map getTrait . traits
artList :: Character -> [ Art ]
artList = filterNothing . map getTrait . traits
spellList :: Character -> [ Spell ]
spellList = filterNothing . map getTrait . traits
reputationList :: Character -> [ Reputation ]
reputationList = filterNothing . map getTrait . traits
ptList :: Character -> [ PTrait ]
ptList = filterNothing . map getTrait . traits
charList :: Character -> [ Characteristic ]
charList = filterNothing . map getTrait . traits
confList :: Character -> [ Confidence ]
confList  = filterNothing . map getTrait . traits
otherList :: Character -> [ OtherTrait ]
otherList  = filterNothing . map getTrait . traits

characterPossessions :: Character -> [ Possession ]
characterPossessions = filterNothing . map getTrait . traits 

labList :: Character -> [ Lab ]
labList  = filterNothing . map getTrait . traits
combatList :: Character -> [ CombatOption ]
combatList  = filterNothing . map getTrait . traits
-- csTraits :: [ Trait ]
-- csTraits = traits
--

-- | Return the character's lab if any
characterLab :: Character -> Maybe Lab
characterLab = f . labList 
   where f [] = Nothing
         f (lb:_) = Just lb

-- | Get a given `Possession` from the character sheet
sheetPossession :: Character -> TraitKey -> Maybe Possession
sheetPossession cs k = ( findTrait k . characterPossessions ) cs

-- | Get the score and speciality in a given ability.
sheetAbilityScore :: Character -> TraitKey -> (Int,Maybe String)
sheetAbilityScore cs k | isNothing x = (0,Nothing)
                     | otherwise = (abilityScore x', speciality x') 
     where x = ( findTrait k . abilityList ) cs
           x' = fromJust x
-- | Get a given art score 
sheetArtScore :: Character -> TraitKey -> Int
sheetArtScore cs k | isNothing x = 0
                 | otherwise = artScore x'
     where x = ( findTrait k . artList ) cs
           x' = fromJust x
-- | Get a given characteristic score 
sheetCharacteristicScore :: Character -> TraitKey -> Int
sheetCharacteristicScore cs k | isNothing x = 0
                 | otherwise = charScore x'
     where x = ( findTrait k . charList ) cs
           x' = fromJust x


-- | Helper for `castingScore`
castingScore' :: Character -> [TraitKey] -> [TraitKey] -> Int
castingScore' cs ts fs = t + f + sta
    where t = minl $  map (sheetArtScore cs) ts
          f = minl $  map (sheetArtScore cs) fs
          sta = sheetCharacteristicScore cs (CharacteristicKey "Sta")
          minl [] = 0
          minl (x:xs) = foldl min x xs

ritualCastingBonus :: Character -> Int
ritualCastingBonus = ritualCastingBonus' "Ritual Casting"
ceremonialCastingBonus :: Character -> Int
ceremonialCastingBonus = ritualCastingBonus' "Ceremonial Casting"

ritualCastingBonus' :: String -> Character -> Int
ritualCastingBonus' sp cs = p + a
    where (p',ps) = sheetAbilityScore cs (AbilityKey "Philosophiae" ) 
          (a',as) = sheetAbilityScore cs (AbilityKey "Artes Liberales" ) 
          a | fromMaybe "" as == sp = a' + 1
            | otherwise = a' 
          p | fromMaybe "" ps == sp = p' + 1
            | otherwise = p' 

-- | Return the Casting Score for a given spell.
-- The function depends both on the Spell trait from the Character
-- and a generic spell description from a SpellDB.
castingScore :: SpellDB    -- ^ Spell DB with general descriptions of the spells
             -> Character  -- ^ Current character sheet
             -> Spell      -- ^ the spell
             -> Int        -- ^ Computed casting score
castingScore db cs spell | isNothing rec' =   0
                     | isNothing sp' =   0
                     | otherwise =  castingScore' cs ts fs + mf (getTrait sp) + rb
   where sp' = findTrait k (traits cs)
         sp = fromJust sp'
         mf Nothing = 0
         mf (Just x) = masteryScore x
         rec' = spellTRecord spell `mplus` spellLookup k db
         rec = fromJust rec'
         ts = (ArtKey $ technique rec):(map ArtKey $ techniqueReq rec)
         fs = (ArtKey $ form rec):(map ArtKey $ formReq rec)
         rb | isRitual rec = ritualCastingBonus cs
            | otherwise = 0
         k = traitKey spell

spellsWithScores :: SpellDB -> Character -> [ Spell ]
spellsWithScores db cs = map (addCastingScore db cs) (spellList cs)
addCastingScore :: SpellDB -> Character -> Spell -> Spell
addCastingScore db cs sp =  sp { spellCastingScore = sc }
   where sc = Just $ castingScore db cs sp 

-- | Return the Lab Total a given TeFo combo.
labTotal :: Character -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotal cs te fo = labTotalBase cs te fo + labTotalBonus cs te fo

-- | Return the Lab Bonuses for a given TeFo.
labTotalBonus :: Character -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotalBonus = labTotalBonus' . characterLab

-- | Return the Lab Bonuses for a given TeFo and Lab.
labTotalBonus' :: Maybe Lab -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotalBonus' (Just lb) (ArtKey te) (ArtKey fo) = g + t + f
    where g = labAura (labState lb) + gq lb
          t = getLabArt te lb 
          f = getLabArt fo lb
labTotalBonus' _ _ _ = 0

-- | Return the Lab Total a given TeFo combo without lab bonuses.
labTotalBase :: Character -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotalBase cs te fo = ts + fs + int + mt
   where ts = sheetArtScore cs te
         fs = sheetArtScore cs fo
         int = sheetCharacteristicScore cs (CharacteristicKey "Int" ) 
         (mt,_) = sheetAbilityScore cs (AbilityKey "Magic Theory" ) 

-- | Lab totals for each TeFo combo.
-- This is used to render a table of lab totals on the character sheet.
labTotals :: Character -- ^ Current character sheet
             -> [[Int]]     -- ^ Computed lab totals 
labTotals cs = [ [ labTotal cs te fo | te <- techniques ] | fo <- forms ]

-- | Casting totals for each TeFo combo.
-- This is used to render a table of casting totals on the character sheet.
castingTotals :: Character -- ^ Current character sheet
             -> [[Int]]     -- ^ Computed casting totals 
castingTotals cs = [ [ castingTotal cs te fo | te <- techniques ] | fo <- forms ]

-- | Return the Lab Total a given TeFo combo.
castingTotal :: Character -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
castingTotal cs te fo = ts + fs + sta 
   where ts = sheetArtScore cs te
         fs = sheetArtScore cs fo
         sta = sheetCharacteristicScore cs (CharacteristicKey "Sta" ) 

-- | List of Hermetic Techniques
techniques :: [ TraitKey ]
techniques = [ ArtKey te | te <- [ "Cr", "In", "Mu", "Pe", "Re" ] ]

-- | List of Hermetic Forms
forms :: [ TraitKey ]
forms = [ ArtKey fo | fo <- [ "An", "Aq", "Au", "Co", "He", "Ig", "Im", "Me", "Te", "Vi" ] ]


-- * Common interface for Character and CharacterState

-- | Class comprising different interfaces to a Character.
-- The class provides convenience functions.  A minimal implementation
-- has to implement `characterSheet`.
class CharacterLike ct where
     -- | The type (Magus/Companion/Grog) of character
     characterType :: ct -> CharacterType
     -- | Is the character a grog or not?
     isGrog :: ct -> Bool
     isGrog c | characterType c == Grog = True
              | otherwise = False
     -- | Is the character a magus or not?
     isMagus :: ct -> Bool
     isMagus c | characterType c == Magus = True
               | otherwise = False
     ageObject :: ct -> Maybe Age
     age :: ct -> Int
     age = fromMaybe (-1) . fmap ageYears . ageObject

instance CharacterLike Character where
     characterType = charType . concept
     ageObject = mhead . filterNothing . map getTrait . traits


-- |
-- = Vis

visList :: Character -> [Possession]
visList = filter (isJust . visArt) . characterPossessions

xVis :: Possession -> (String,Int)
xVis p | isNothing (visArt p) = ("",0)
       | otherwise = (fromJust $ visArt p,itemCount p)

addVis :: [(String,Int)] -> [(String,Int)]
addVis [] = []
addVis (("",_):xs) = addVis xs
addVis (x:[]) = [x]
addVis (x:y:xs) | fst x == fst y = addVis $ (fst x, snd x+snd y):xs
                | otherwise = x:(addVis $ y:xs)

sheetVis :: Character -> [(TraitKey,Int)]
sheetVis = sortOn fst . map f . addVis . sort . map xVis . visList
    where f (x,y) = (ArtKey $ take 2 x,y) 

-- | 
-- = Derived stats 

-- |
-- Calculate the Source Quality the character generates as a teacher.
charTeacherSQ :: Character -> Int
charTeacherSQ cs = 3 + com + tch
    where com = sheetCharacteristicScore cs (CharacteristicKey "Com")
          (tch,_) = sheetAbilityScore cs (CharacteristicKey "Teaching")
          -- add good teacher
          -- subtract flaws
          -- add speciality
          -- add one/two student bonus
-- Teacher SQ +
