{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.CharacterSheet
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
module ArM.Char.CharacterSheet ( CharacterSheet(..)
                               , sheetArtScore
                               , sheetAbilityScore
                               , sheetCharacteristicScore
                               , sheetPossession
                               , castingScore
                               , addCastingScores
                               , castingTotals
                               , labTotals
                               , CharacterLike(..)
                               , sheetVis
                               , characterLab
                               , charTeacherSQ
                               , ritualCastingBonus
                               , ceremonialCastingBonus
                               ) where

import ArM.Types.ProtoTrait
import ArM.Types.Character
import ArM.Types.Lab
import ArM.DB.Spell
import ArM.Helper

import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.List

-- import ArM.Debug.Trace

-- | The CharacterSheet object holds a Character State with separate fields
-- for different kinds of traits
data CharacterSheet = CharacterSheet 
         { csType :: CharacterType
         , vfList :: [ VF ]
         , charList :: [ Characteristic ]
         , abilityList :: [ Ability ]
         , artList :: [ Art ]
         , spellList :: [ Spell ]
         , reputationList :: [ Reputation ]
         , ptList :: [ PTrait ]
         , confList :: [ Confidence ]
         , otherList :: [ OtherTrait ]
         , possessionList :: [ Possession ]
         , labList :: [ Lab ]
         , combatList :: [ CombatOption ]
         , csTraits :: [ Trait ]
         }  deriving (Eq,Show,Generic)

-- | Return the character's lab if any
characterLab :: CharacterLike c => c -> Maybe Lab
characterLab = f . labList . characterSheet
   where f [] = Nothing
         f (lb:_) = Just lb

-- | A default CharacterSheet for internal use.
defaultSheet :: CharacterSheet 
defaultSheet = CharacterSheet 
         { csType = Magus
         , vfList = [ ]
         , charList = [ ]
         , abilityList = [ ]
         , artList = [ ]
         , spellList = [ ]
         , reputationList = []
         , ptList = []
         , confList = []
         , otherList = [ ]
         , possessionList = [ ]
         , labList = [ ]
         , combatList = []
         , csTraits = [ ]
         }  


instance ToJSON CharacterSheet where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CharacterSheet 


-- | Get the CharacterSheet corresponding to a given CharacterState.
filterCS :: CharacterState -> CharacterSheet
filterCS cs = defaultSheet  
                 { csType = charSType cs
                 , vfList = x1
                 , abilityList = x2
                 , artList = x3
                 , spellList = sortTraits x4
                 , reputationList = x5
                 , ptList = x6
                 , charList = x7
                 , confList = x8
                 , otherList = x9
                 , possessionList = x10
                 , labList = x11
                 , combatList = x12
                 , csTraits = y12
                }
           where (x1,y1) = filterTrait $ traits cs
                 (x2,y2) = filterTrait y1
                 (x3,y3) = filterTrait y2
                 (x4,y4) = filterTrait y3
                 (x5,y5) = filterTrait y4
                 (x6,y6) = filterTrait y5
                 (x7,y7) = filterTrait y6
                 (x8,y8) = filterTrait y7
                 (x9,y9) = filterTrait y8
                 (x10,y10) = filterTrait y9
                 (x11,y11) = filterTrait y10
                 (x12,y12) = filterTrait y11



-- | Find a trait, given by a key, from the CharacterSheet.
-- This may not be in use. 
findTraitCS ::  TraitKey -> CharacterSheet -> Maybe Trait
findTraitCS (AbilityKey x) = (fmap toTrait) . findTrait (AbilityKey x) . abilityList
findTraitCS (ArtKey x) = (fmap toTrait) . findTrait (ArtKey x) . artList
findTraitCS (SpellKey x y z) = (fmap toTrait) . findTrait (SpellKey x y z) . spellList
findTraitCS (CharacteristicKey x) = (fmap toTrait) . findTrait (CharacteristicKey x) . charList
findTraitCS _ = \ _ -> Nothing

-- | Get a given `Possession` from the character sheet
sheetPossession :: CharacterSheet -> TraitKey -> Maybe Possession
sheetPossession cs k = ( findTrait k . possessionList ) cs

-- | Get the score and speciality in a given ability.
sheetAbilityScore :: CharacterSheet -> TraitKey -> (Int,Maybe String)
sheetAbilityScore cs k | isNothing x = (0,Nothing)
                     | otherwise = (abilityScore x', speciality x') 
     where x = ( findTrait k . abilityList ) cs
           x' = fromJust x
-- | Get a given art score 
sheetArtScore :: CharacterSheet -> TraitKey -> Int
sheetArtScore cs k | isNothing x = 0
                 | otherwise = artScore x'
     where x = ( findTrait k . artList ) cs
           x' = fromJust x
-- | Get a given characteristic score 
sheetCharacteristicScore :: CharacterSheet -> TraitKey -> Int
sheetCharacteristicScore cs k | isNothing x = 0
                 | otherwise = charScore x'
     where x = ( findTrait k . charList ) cs
           x' = fromJust x


-- | Helper for `castingScore`
castingScore' :: CharacterSheet -> [TraitKey] -> [TraitKey] -> Int
castingScore' cs ts fs = t + f + sta
    where t = minl $  map (sheetArtScore cs) ts
          f = minl $  map (sheetArtScore cs) fs
          sta = sheetCharacteristicScore cs (CharacteristicKey "Sta")
          minl [] = 0
          minl (x:xs) = foldl min x xs

ritualCastingBonus :: CharacterSheet -> Int
ritualCastingBonus = ritualCastingBonus' "Ritual Casting"
ceremonialCastingBonus :: CharacterSheet -> Int
ceremonialCastingBonus = ritualCastingBonus' "Ceremonial Casting"

ritualCastingBonus' :: String -> CharacterSheet -> Int
ritualCastingBonus' sp cs = p + a
    where (p',ps) = sheetAbilityScore cs (AbilityKey "Philosophiae" ) 
          (a',as) = sheetAbilityScore cs (AbilityKey "Artes Liberales" ) 
          a | fromMaybe "" as == sp = a' + 1
            | otherwise = a' 
          p | fromMaybe "" ps == sp = p' + 1
            | otherwise = p' 

-- | Return the Casting Score for a given spell.
-- The function depends both on the Spell trait from the CharacterSheet
-- and a generic spell description from a SpellDB.
castingScore :: SpellDB    -- ^ Spell DB with general descriptions of the spells
             -> CharacterSheet -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the spell
             -> Int            -- ^ Computed casting score
castingScore db cs k | isNothing rec' =   0
                     | isNothing sp' =   0
                     | otherwise =  castingScore' cs ts fs + mf ( getTrait sp) + rb
   where sp' = findTraitCS k cs
         sp = fromJust sp'
         mf Nothing = 0
         mf (Just x) = masteryScore x
         rec' = spellLookup k db
         rec = fromJust rec'
         ts = (ArtKey $ technique rec):(map ArtKey $ techniqueReq rec)
         fs = (ArtKey $ form rec):(map ArtKey $ formReq rec)
         rb | isRitual rec = ritualCastingBonus cs
            | otherwise = 0

addCastingScores :: SpellDB -> CharacterSheet -> CharacterSheet
addCastingScores db cs =  cs { spellList = spellList' }
   where spellList' = map (addCastingScore db cs) (spellList cs)
addCastingScore :: SpellDB -> CharacterSheet -> Spell -> Spell
addCastingScore db cs sp =  sp { spellCastingScore = sc }
   where sc = Just $ castingScore db cs (traitKey sp) 


-- | Return the Lab Total a given TeFo combo.
labTotal :: CharacterSheet -- ^ Current character sheet
             -> TraitKey       -- ^ Key identifying the technique
             -> TraitKey       -- ^ Key identifying the form
             -> Int            -- ^ Computed lab total
labTotal cs te fo = labTotalBase cs te fo + labTotalBonus cs te fo

-- | Return the Lab Bonuses for a given TeFo.
labTotalBonus :: CharacterSheet -- ^ Current character sheet
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
labTotalBase :: CharacterSheet -- ^ Current character sheet
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
labTotals :: CharacterSheet -- ^ Current character sheet
             -> [[Int]]     -- ^ Computed lab totals 
labTotals cs = [ [ labTotal cs te fo | te <- techniques ] | fo <- forms ]

-- | Casting totals for each TeFo combo.
-- This is used to render a table of casting totals on the character sheet.
castingTotals :: CharacterSheet -- ^ Current character sheet
             -> [[Int]]     -- ^ Computed casting totals 
castingTotals cs = [ [ castingTotal cs te fo | te <- techniques ] | fo <- forms ]

-- | Return the Lab Total a given TeFo combo.
castingTotal :: CharacterSheet -- ^ Current character sheet
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


-- |
-- = Common interface for Character, CharacterSheet, and CharacterState

-- | Class comprising different interfaces to a Character.
-- The class provides convenience functions.  A minimal implementation
-- has to implement `characterSheet`.
class CharacterLike ct where
     -- | The type (Magus/Companion/Grog) of character
     characterType :: ct -> CharacterType
     characterType = characterType . characterSheet
     -- | Is the character a grog or not?
     isGrog :: ct -> Bool
     isGrog c | characterType c == Grog = True
              | otherwise = False
     -- | Is the character a magus or not?
     isMagus :: ct -> Bool
     isMagus c | characterType c == Magus = True
               | otherwise = False
     -- | Character age in years
     age :: ct -> Int
     age = age . characterSheet
     -- | The age object (trait) of the character
     ageObject  ::  ct -> Maybe Age
     ageObject = ageObject . characterSheet
     -- | The `CharacterSheet` objct representing the character (state)
     characterSheet :: ct -> CharacterSheet
instance CharacterLike Character where
     characterType = charType . concept
     characterSheet c | isNothing st = defaultSheet { csType = charType (concept c) }
                      | otherwise = cs { csType = charType (concept c) }
         where st = state c
               cs = filterCS st'
               st' = fromJust st 
instance CharacterLike CharacterState where
     characterType = charSType 
     characterSheet = filterCS
instance CharacterLike CharacterSheet where
    characterType = csType
    age = f . ageObject
      where f Nothing = -1
            f (Just x) = ageYears  x
    ageObject = maybeHead . fst . filterTrait . csTraits
    characterSheet = id

-- |
-- = Vis

visList :: CharacterSheet -> [Possession]
visList = filter (isJust . visArt) . possessionList

xVis :: Possession -> (String,Int)
xVis p | isNothing (visArt p) = ("",0)
       | otherwise = (fromJust $ visArt p,itemCount p)

addVis :: [(String,Int)] -> [(String,Int)]
addVis [] = []
addVis (("",_):xs) = addVis xs
addVis (x:[]) = [x]
addVis (x:y:xs) | fst x == fst y = addVis $ (fst x, snd x+snd y):xs
                | otherwise = x:(addVis $ y:xs)

sheetVis :: CharacterSheet -> [(TraitKey,Int)]
sheetVis = sortOn fst . map f . addVis . sort . map xVis . visList
    where f (x,y) = (ArtKey $ take 2 x,y) 

-- | 
-- = Derived stats 

-- |
-- Calculate the Source Quality the character generates as a teacher.
charTeacherSQ :: CharacterLike c => c -> Int
charTeacherSQ cs = 3 + com + tch
    where sheet = characterSheet cs
          com = sheetCharacteristicScore sheet (CharacteristicKey "Com")
          (tch,_) = sheetAbilityScore sheet (CharacteristicKey "Teaching")
          -- add good teacher
          -- subtract flaws
          -- add speciality
          -- add one/two student bonus
-- Teacher SQ +
