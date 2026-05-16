{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Internal.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types for Character Traits and Possessions.
--
-- This module defines a range of mutually dependent types, which cannot (generally)
-- be split across modules because of these inter-dependencies.
--
-- The types cover not only character traits, but also possessions which apply to
-- both covenants and individual characters.  Books and enchanted devices are
-- examples of possessions.
--
-- This module proves a type for each kind of trait as well as a wrapper type,
-- `Trait` which can represent any kind of trait.
--
-- Possessions are autonomous objects, which may be transferred.  Poessions of
-- characters are wrapped as a trait.
--
-- Books are represented as `Book` objects for the sake of reading, but as possesions
-- they are wrapped as `Possession` object which may represent antologies.  The same
-- is the case for `LabText` objects.
--
-----------------------------------Types.------------------------------------------
module ArM.Types.Internal.Trait ( 
         -- * The Trait Types
         Trait(..)
         , Ability(..)
         , Characteristic(..)
         , Art(..)
         , VF(..)
         , Spell(..)
         , PTrait(..)
         , OtherTrait(..)
         , CombatOption(..)
         , Confidence(..)
         , Reputation(..)
         , Age(..)
         -- * Convenience Functions
         , TraitClass(..)
         -- * Aging
         , module ArM.Types.Internal.Aging
         -- * TraitKey
         , module ArM.Types.Internal.TraitKey
         -- * Books
         , BookStats(..)
         , Book(..)
         , defaultBook
         , BookDB(..)
         -- * Possessions
         , Possession(..)
         , defaultPossession
         , LabText(..)
         , visArt
         , isAC
         , isNone
         -- ** Magic and Enchantments
         , Enchantment(..)
         , MagicEffect(..)
         ) where


import ArM.GameRules
import ArM.Helper
import ArM.Types.Internal.TraitKey
import ArM.Types.Internal.Aging
import ArM.Types.HarmObject
import ArM.Types.Calendar
import ArM.Types.Lab
import ArM.DB.Spell
import ArM.DB.Weapon

import GHC.Generics
import Data.Aeson
import Data.Aeson.Extra
-- import Data.Aeson.Key
import Data.Aeson.Types
import Data.Text.Lazy  ( fromStrict, unpack )
import Data.Maybe
-- import Data.Text       (splitOn)
-- import Text.Read 
import Control.Monad
-- import Control.Applicative ((<|>))

-- * The Trait Type

-- | The `Trait` type represents any kind of character trait, including
-- equipment and other possessions.
data Trait = AbilityTrait Ability
           | CharacteristicTrait Characteristic
           | ArtTrait Art
           | SpellTrait Spell
           | PTraitTrait PTrait         -- ^ Personality Trait
           | ReputationTrait Reputation
           | VFTrait VF                 -- ^ Virtue or flaw
           | ConfidenceTrait Confidence 
                   -- ^ Trait with score and points (Confidence, True Faith, etc.)
           | OtherTraitTrait OtherTrait
                   -- ^ Other traits advancing like abilities, e.g. Warping and Decrepitude
           | PossessionTrait Possession -- ^ Movable and transferable property
           | EstateTrait Lab            -- ^ Immovable property, typically lab
           | CombatOptionTrait CombatOption  
                   -- ^ Configuration for which combat stats should be calculated
           | AgeTrait Age
           deriving (Show, Eq, Generic)
instance Ord Trait where
     compare x y = compare (traitKey x) (traitKey y)

-- | Is the trait included with a zero count?
isNone :: Trait -> Bool
isNone (VFTrait x) = count x == 0
isNone (PossessionTrait x) = count x == 0
isNone _ = False

-- ** Different types of Traits

data Ability = Ability { abilityName :: String
                       , speciality :: Maybe String
                       , abilityXP :: XPType 
                       , abilityScore :: Int 
                       , abilityBonus :: Int 
                       , abilityMultiplier :: Float
                       , abilityExcessXP :: XPType 
                       }
           deriving (Ord, Eq, Generic)
data Characteristic = Characteristic { characteristicName :: String
                                     , charScore :: Int
                                     , agingPoints :: Int 
                                     , charBonusList :: [(Int,Int)] 
                                     }
           deriving (Eq, Generic)
instance Ord Characteristic where
     compare x y = compare (traitKey x) (traitKey y)
data Art = Art { artName :: String
               , artXP :: XPType 
               , artScore :: Int 
               , artBonus :: Int 
               , artMultiplier :: Float
               , artExcessXP :: XPType 
               }
           deriving (Eq, Generic)
instance Ord Art where
     compare x y = compare (traitKey x) (traitKey y)

data Spell = Spell { spellName :: String
                   , spellTeFo :: String
                   , spellLevel :: Int
                   , spellXP :: XPType
                   , masteryScore :: Int
                   , spellExcessXP :: XPType
                   , spellMultiplier :: Float
                   , masteryOptions :: [String] 
                   , spellCastingScore :: Maybe Int
                   , spellTComment :: String
                   , spellTRecord :: Maybe SpellRecord
                   }
           deriving (Ord, Eq, Generic)

-- | Return a string of Form/Technique for sorting
spellFoTe :: Spell -> String
spellFoTe = fote . spellTeFo 


-- | Personality trait
data PTrait = PTrait { ptraitName :: String, pscore :: Int }
           deriving (Ord, Eq, Generic)

-- | Reputation object 
data Reputation = Reputation { reputationName :: String  -- ^ contents of the reputation
                             , repLocale :: String       -- ^ domain or location of the reputation
                             ,  repXP :: XPType          -- ^ total XP in the reputation (used?)
                             ,  repScore :: Int          -- ^ reputation Score
                             ,  repExcessXP :: XPType    -- ^ XP towards next level in the reputation
                             }
           deriving (Ord, Eq, Generic)
data VF = VF { vfname :: String    -- ^ name of the virtue/flaw
             , vfDetail :: String  -- ^ detail, where the virtue/flaw has options
             , vfcost :: Int       -- ^ cost, should be zero for free/inferred virtues/flaws
             , vfAppliesTo :: Maybe TraitKey  -- ^ not used
             , vfMultiplicity :: Int          -- ^ number of times the virtue/flaw is take
             , vfComment :: String              -- ^ freeform comment
             }
           deriving (Ord, Eq, Generic)
instance Countable VF where
    count = vfMultiplicity
    addCount x n = x { vfMultiplicity = vfMultiplicity x + n }
-- | The Confidence trait covers True Faith as well as Confidence,
-- and potentially other traits where points are accumulated without
-- limit and independently of the score.
data Confidence = Confidence { cname :: String, cscore :: Int, cpoints :: Int }
           deriving ( Ord, Eq, Generic)
-- | OtherTrait covers warping and decrepitude, and potentially other singular
-- traits which progress like abilities.
data OtherTrait = OtherTrait { trait :: String
                             , otherScore :: Int
                             -- , pts :: Int 
                             , otherExcess :: Int
                             }
           deriving (Ord, Eq, Generic)


-- ** Show instances

instance Show VF  where
   show a = vfname a ++ f sp ++ " (" ++ cst ++ ")"
      where sp = vfDetail a
            f "" = ""
            f x = " [" ++ x ++ "]"
            cst | m == 1 = show (vfcost a) 
                | otherwise = show (vfcost a) ++ "x" ++ show m
            m = vfMultiplicity a
instance Show Confidence  where
   show a = cname a ++ ": " ++ show (cscore a) ++ " (" ++ show (cpoints a) ++ ")"
instance Show OtherTrait  where
   show a = trait a ++ ": " ++ show (otherScore a) ++ " (" ++ show (otherExcess a) ++ ")"
instance Show PTrait  where
   show a = ptraitName a ++ " " ++ show (pscore a)
instance Show Ability  where
   show a = abilityName a ++ " [" ++ showspec sp ++ "] "
          ++ show (abilityScore a) 
          ++ showBonus (abilityBonus a)
          ++ " (" ++ showNum (abilityExcessXP a) ++ "xp)"
          ++ f (abilityMultiplier a)
      where showspec Nothing = "  --  "
            showspec (Just s) = s
            sp = speciality a
            f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Characteristic  where
   show a = characteristicName a ++ " " ++ showSigned (charScore a)
          ++ showA (agingPoints a)
       where showA x | x == 0 = ""
                    | otherwise = " (" ++ show x ++ " aging points)"
instance Show Spell  where
   show a = "*" ++ spellName a ++ "* " 
            ++ spellTeFo a ++ show (spellLevel a) ++ f (spellCastingScore a)
      where f Nothing = ""
            f (Just x) = " (" ++ show x ++ ")"
instance Show Art  where
   show a = artName a ++ " " 
          ++ show (artScore a) 
          ++ showBonus (artBonus a)
          ++ " (" ++ showNum (artExcessXP a) ++ "xp) "
          ++ f (artMultiplier a)
      where f 1 = ""
            f x = " [xp x" ++ show x ++  "]"
instance Show Reputation where
   show a = reputationName a ++ " [" ++ (repLocale a) ++ "] "
          ++ show (repScore a) ++ " (" ++ showNum (repExcessXP a) ++ ") "

-- ** Combat Options

-- | A CombatOption is a combination of weapons for which to list combat stats.
--
-- It needs to link with a `Weapon` (and optionally a shield) which may be
-- generic or unique.  If the weapon can be used in different modes, the
-- ability has to be linked as well.
data CombatOption = CombatOption 
     { combatName :: String          -- ^ Describing Name of the weapon combination
     , combatWeapon :: String        -- ^ The main weapon
     , combatShield :: Maybe String  -- ^ A Shield is optional
     , combatAbility :: Maybe String
     }  deriving (Eq,Ord,Generic)

instance Show CombatOption where
   show co = "Combat Option: " ++ combatName co ++ ab ++ " " ++ (combatWeapon co) ++ sh
      where ab | isNothing (combatAbility co) = ""
               | otherwise = " (" ++ (fromJust $ combatAbility co) ++ ")"
            sh | isNothing (combatShield co) = ""
               | otherwise = "/" ++ (fromJust $ combatShield co) 

instance ToJSON CombatOption
instance FromJSON CombatOption where
    parseJSON = fmap f . parseJSON'
      where f p | combatName p == "" = p { combatName = combatWeapon p }
                | otherwise = p
            parseJSON' = withObject "CombatOption" $ \v -> CombatOption
                    <$> v .:?  "name" .!= ""
                    <*> v .:   "weapon"
                    <*> v .:?  "shield"
                    <*> v .:?  "ability"
-- | 'TraitClass' provides the functions to get the search key (TraitKey),
-- to wrap and unwrap traits in the generic `Trait` type, and to filter
-- traits of different types.
-- 
-- 'ProtoTrait' and its constituent types may also implement TraitClass
-- but `getTrait` may then always return Nothing.
class TraitClass t where
    -- | Get the key of the trait
    traitKey :: t -> TraitKey
    -- | Wrap the trait as a generic `Trait` object.
    toTrait :: t -> Trait
    -- | Return the specific trait from the generic Trait,
    -- or Nothing if the type does not match.
    getTrait :: Trait -> Maybe t

    -- | Extract traits of the given type from a generic list of Trait objects.
    -- It returns a pair of lists with the selected traits in the first list
    -- and the remaining traits in the other.
    filterTrait :: [ Trait ] -> ( [ t ], [ Trait ] )
    filterTrait ts = y where (_,y) = filterTrait' (ts,([],[]))

    -- | Recursive helper for `filterTrait`
    filterTrait' :: ( [ Trait ], ( [ t ], [ Trait ] ) )
                  -> ( [ Trait ], ( [ t ], [ Trait ] ) )
    filterTrait' ([],y) = ([],y)
    filterTrait' (x:xs,(ys,zs)) | isNothing ab  = filterTrait' (xs,(ys,x:zs))
                                | otherwise = filterTrait' (xs,(fromJust ab:ys,zs))
        where ab = getTrait x


instance TraitClass Trait where
    traitKey (CharacteristicTrait x) = traitKey x
    traitKey (AbilityTrait x) = traitKey x
    traitKey (ArtTrait x) = traitKey x
    traitKey (SpellTrait x) = traitKey x
    traitKey (ReputationTrait x) = traitKey x
    traitKey (VFTrait x) = traitKey x
    traitKey (PTraitTrait x) = traitKey x
    traitKey (OtherTraitTrait x) = traitKey x
    traitKey (ConfidenceTrait x) = traitKey x
    traitKey (PossessionTrait x) = traitKey x
    traitKey (EstateTrait x) = traitKey x
    traitKey (CombatOptionTrait x) = traitKey x
    traitKey (AgeTrait x) = traitKey x
    toTrait = id
    getTrait = Just . id

instance TraitClass Ability where
    traitKey x = AbilityKey $ abilityName x
    toTrait = AbilityTrait
    getTrait (AbilityTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Art where
    traitKey x = ArtKey $ take 2 $ artName x
    toTrait = ArtTrait
    getTrait (ArtTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Spell where
    traitKey x = SpellKey (spellFoTe x) (spellLevel x) (spellName x ) 
    toTrait = SpellTrait
    getTrait (SpellTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass VF where
    traitKey x = VFKey (vfname x) (vfDetail x)
    toTrait = VFTrait
    getTrait (VFTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass PTrait where
    traitKey x = PTraitKey $ ptraitName x
    toTrait = PTraitTrait
    getTrait (PTraitTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Reputation where
    traitKey x = ReputationKey ( reputationName x ) ( repLocale x )
    toTrait = ReputationTrait
    getTrait (ReputationTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Characteristic where
    traitKey x = CharacteristicKey ( characteristicName x ) 
    toTrait = CharacteristicTrait
    getTrait (CharacteristicTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass Confidence where
    traitKey p = ConfidenceKey $ cname p
    toTrait = ConfidenceTrait
    getTrait (ConfidenceTrait x) = Just x
    getTrait _ = Nothing
instance TraitClass OtherTrait where
    traitKey x = OtherTraitKey ( trait x ) 
    toTrait = OtherTraitTrait
    getTrait (OtherTraitTrait x) = Just x
    getTrait _ = Nothing

instance TraitClass Age where
    traitKey _ = AgeKey
    toTrait = AgeTrait
    getTrait (AgeTrait x) = Just x
    getTrait _ = Nothing

instance TraitClass Possession where
    traitKey x = PossessionKey $ name x
    getTrait (PossessionTrait x) = Just x
    getTrait _ = Nothing
    toTrait = PossessionTrait
instance TraitClass Lab where
    traitKey x = EstateKey $ name x
    getTrait (EstateTrait x) = Just x
    getTrait _ = Nothing
    toTrait = EstateTrait

instance TraitClass CombatOption where
    traitKey x = CombatKey $ combatName x
    getTrait (CombatOptionTrait x) = Just x
    getTrait _ = Nothing
    toTrait = CombatOptionTrait


-- ** Class instances

instance FromJSON Ability
instance FromJSON Characteristic 
instance FromJSON Art 
instance FromJSON Spell 
instance FromJSON PTrait 
instance FromJSON Reputation 
instance FromJSON VF 
instance FromJSON Confidence 
instance FromJSON OtherTrait 
instance FromJSON Trait  
instance ToJSON Ability
instance ToJSON Characteristic 
instance ToJSON Art 
instance ToJSON Spell 
instance ToJSON PTrait 
instance ToJSON Reputation 
instance ToJSON VF 
instance ToJSON Confidence 
instance ToJSON OtherTrait 
instance ToJSON Trait 



-- * Types

-- | The stats of a book as required for advancement mechanics.
data BookStats = BookStats
         { topic :: TraitKey
         , bookLevel :: Maybe Int
         , quality :: Maybe Int
         , reread :: Int          
            -- ^ Number of tractatus in the text.  This is normally 1
            -- and ignored for any text but tractatus, but there are a
            -- few canon examples of texts that count as multiple tractatus.
       }  deriving (Eq,Generic,Ord)
instance ToJSON BookStats
instance FromJSON BookStats where
    parseJSON = withObject "BookStats" $ \v -> BookStats
        <$> v .:? "topic" .!= NoTrait
        <*> v .:? "level" 
        <*> v .:? "quality" 
        <*> v .:? "reread"  .!= 1
instance Show BookStats where
    show b = k ++ ' ':l ++ q
        where k = show $ topic b
              q | isNothing (quality b) = ""
                | otherwise = 'Q':show (fromJust $ quality b)
              l | isNothing (bookLevel b) = ""
                | otherwise = 'L':show (fromJust $ bookLevel b)
{-
instance Ord BookStats where
    compare a b | topic a /= topic b = compare (topic a) (topic b)
                | bookLevel a /= bookLevel b = compare (bookLevel a) (bookLevel b)
                | otherwise  = compare (quality a) (quality b)
-}
-- | A book is an original manuscript.  Antologies and copies are
-- handled as Possession objects.
--
-- A book may have one or more `BookStat` values.  A copy may or may
-- not have book stats.  If it does not, it inherits stats from the original.
data Book = Book
     { bookID :: String
     , bookTitle :: String
     , bookStats :: [ BookStats ] -- ^ list of stats per topic covered
     , bookAuthor :: String      -- ^ Creator of the copy or manuscript
     , bookDate :: SeasonTime     -- ^ Time the copy was made            
     , bookLocation :: Maybe String     -- ^ Location where the book was written or copied
     , bookNarrative :: [ String ]   -- ^ Additional information in free text
     , bookAnnotation :: [ String ]   -- ^ Additional information in free text
     , bookLanguage  :: Maybe String  -- ^ Language of the book
     , bookCount :: Int               -- ^ Number of copies 
     } deriving (Eq,Generic,Show)
instance Ord Book where
    compare a b | bookStats a /= bookStats b = compare (bookStats a) (bookStats b)
                | otherwise = compare (bookTitle a) (bookTitle b)
instance Countable Book where
    count = bookCount
    addCount b n = b { bookCount = bookCount b + n }
instance KeyObject Book where
   harmKey = BookKey . bookID
instance StoryObject Book where
    name book = tis ++ aus ++ dat
     where aut = trim $ bookAuthor book
           aus | aut == "" = ""
               | otherwise = " by " ++ aut
           tit' = trim $ bookTitle book
           tit | tit' /= "" = tit'
               | otherwise = fromMaybe "No title" $ mhead bks
           bks = map show (bookStats book)
           tis | tit == "" = ""
               | otherwise = "*" ++ tit ++ "*"
           dat = " (" ++ show (bookDate book) ++ ")"
    narrative = bookNarrative
    comment = bookAnnotation
instance ToJSON Book
instance FromJSON Book where
    parseJSON = withObject "Book" $ \v -> Book
        <$> v .:? "bookID" .!= "No ID"
        <*> v .:? "title" .!= "No title"
        <*> v `parseCollapsedList` "stats" 
        <*> v .:? "creator" .!= "N/A"
        <*> v .:? "date" .!= NoTime
        <*> v .:? "location" 
        <*> v  `parseCollapsedList` "narrative" 
        <*> v  `parseCollapsedList` "comment" 
        <*> v .:? "language" 
        <*> v .:? "count"  .!= 1


-- | The `BookDB` class is any type wherein one may look up books by
-- their ID.
class BookDB h where
   -- | Look up a book by key (String) in a database.
   bookLookup :: h -> String -> Maybe Book
   bookLookup db k = lookupBook k db 
   -- | Look up a book by key (String) in a database.
   -- This is equivalent to `bookLookup` with the arguments swapped
   lookupBook :: String -> h -> Maybe Book
   lookupBook k db = bookLookup db k

instance (BookDB h) => BookDB [h] where
   lookupBook k = foldl mplus Nothing . map (\ x -> bookLookup x k) 
instance BookDB Book where
   bookLookup bk k | k == bookID bk = Just bk
                   | otherwise = Nothing

-- | A default book object, providing defaults for fields not available in the CSV format.
defaultBook :: Book
defaultBook = Book
     { bookID = ""
     , bookTitle = ""
     , bookStats = [ ] 
     , bookAuthor = ""
     , bookDate = NoTime
     , bookLocation = Nothing
     , bookNarrative = []
     , bookAnnotation = []
     , bookLanguage = Nothing
     , bookCount = 1 }


-- * Descriptions of a reading season

-- | Currently unused, this is one idea for describing what book and part
-- is read in a given season.
data ReadingID = ReadingID
     { bookRead :: HarmKey
     , partRead :: Maybe HarmKey
     , topicRead :: TraitKey
     } deriving (Eq,Show,Generic)
instance ToJSON ReadingID
instance FromJSON ReadingID 


-- * Possession

-- |
-- == Weapons and other Possessions


instance KeyObject Possession where
   harmKey = ItemKey . itemName


-- | A `Possession` is any kind of device that can be acquired, lost,
-- given, or traded.  It is treated like inherent traits in the data
-- model.  Possessions comprise weapons, armour, vis, magic devices,
-- equipment, and any physical object that should be recorded
-- on the characters sheet.
data Possession = Possession 
     { itemName :: String            -- ^ Name identifying the unique item
     , bookTexts :: [ Book ]         -- ^ List of included texts, if the item is a Book
     , qualityBonus :: Int   
          -- ^ quality bonus applies to book stats when a copy has non-standard
          -- quality due to fast copying, high skill, or other factors.
     , labTexts :: [ LabText ]       -- ^ List of lab texts in the iten (scroll/book)
     , weaponStats :: [ Weapon ]     -- ^ List of applicable Weapon stat objects
     , weapon :: [ String ]          -- ^ List of standard weapon stats that apply
     , armourStats :: [ Armour ]     -- ^ List of applicable Weapon stat objects
     , armour :: [ String ]          -- ^ List of standard weapon stats that apply
     , enchantment :: Enchantment
     , itemDescription :: [ String ] -- ^ Description of the Item
     , itemComment :: [ String ]     -- ^ Comments, supplementing the description
     , itemArt :: Maybe String       -- ^ Relevant art if the item is raw vis
     , acTo :: Maybe String
     , itemCount :: Int              -- ^ Number of items possessed, default 1.
     , itemDate :: SeasonTime        -- ^ Time of creation
     }
    deriving ( Ord, Eq, Generic )
defaultPossession :: Possession 
defaultPossession = Possession
     { itemName = "No Name"
     , bookTexts = []
     , qualityBonus = 0
     , labTexts = []
     , weaponStats = []
     , weapon = []
     , armourStats = []
     , armour = []
     , enchantment = MundaneItem
     , itemDescription = []
     , itemComment = []
     , itemArt = Nothing
     , acTo = Nothing
     , itemCount = 1
     , itemDate = NoTime
     }

data Enchantment = LesserItem MagicEffect
                 | GreaterDevice Int [ MagicEffect ]
                 | Talisman Int [ MagicEffect ]
                 | ChargedItem Int MagicEffect
                 | MundaneItem
    deriving ( Ord, Eq, Generic )

instance ToJSON Enchantment 

enchantmentName :: Enchantment -> String
enchantmentName (LesserItem e) = effectName e
enchantmentName (ChargedItem _ e) = effectName e
enchantmentName (GreaterDevice _ (e:_)) = effectName e
enchantmentName (Talisman _ _) = "Talisman"
enchantmentName _ = ""

{-
parseLesser :: Object -> Parser Enchantment
parseLesser = fmap LesserItem . f . KM.lookup "lesseritem"
    where f Nothing = mzero
          f (Just x) = parseJSON x
-}

parseLesser :: Object -> Parser Enchantment
parseLesser v = LesserItem
        <$> v .: "lesseritem" 

parseGreater :: Object -> Parser Enchantment
parseGreater v = GreaterDevice
        <$> v .: "viscapacity" 
        <*> v `parseCollapsedList` "effects" 
parseTalisman :: Object -> Parser Enchantment
parseTalisman v = GreaterDevice
        <$> v .: "talisman" 
        <*> v `parseCollapsedList` "effects" 
parseCharged :: Object -> Parser Enchantment
parseCharged v = ChargedItem
        <$> v .: "charged" 
        <*> v .: "effect" 

instance FromJSON Enchantment where
    parseJSON (Object v) = foldl mplus (parseLesser v) 
       [ (parseGreater v), (parseTalisman v), (parseCharged v) ]
    parseJSON _ = mzero

visArt :: Possession -> Maybe String
visArt = itemArt 

-- == Books


instance StoryObject MagicEffect where
   name ob = effectName ob 
   setName n x = x { effectName = n }
   narrative ob = effectDescription ob
   addNarrative s x = x { effectDescription = s:narrative x }
   comment ob = effectComment ob
   addComment s x = x { effectComment = s:comment x }

instance StoryObject Possession where
   name ob = itemName ob 
   setName n x = x { itemName = n }
   narrative ob = itemDescription ob
   addNarrative s x = x { itemDescription = s:narrative x }
   comment ob = comment ob
   addComment s x = x { itemComment = s:comment x }

instance Countable Possession where
   count ob = itemCount ob
   addCount ob n  = ob { itemCount = itemCount ob + n }

instance ToJSON Possession 

instance FromJSON Possession where
    parseJSON (String t) = pure $ setName (unpack (fromStrict t)) defaultPossession 
    parseJSON (Object v) = (parseOtherPossession v)
    parseJSON _ = mzero

parseOtherPossession :: Object -> Parser Possession
parseOtherPossession v = fmap fixPossessionName $ Possession 
       <$> v .:? "name" .!= ""
       <*> v `parseCollapsedList` "books" 
       <*> v .:? "bonus" .!= 0
       <*> v `parseCollapsedList` "labtexts" 
       <*> v .:? "weaponStats" .!= []
       <*> v `parseCollapsedList` "weapon"
       <*> v .:? "armourStats" .!= []
       <*> v `parseCollapsedList` "armour"
       <*> v .:? "enchantment" .!= MundaneItem
       <*> v `parseCollapsedList` "description" 
       <*> v `parseCollapsedList` "comment" 
       <*> v .:? "art"
       <*> v .:? "acTo" 
       <*> v .:? "count" .!= 1
       <*> v .:? "date"  .!= NoTime



-- | Derive `itemName` from other properties, if the name is undefined.
fixPossessionName :: Possession -> Possession 
fixPossessionName =  fixPN getPN1 . fixPN f
    where f p | enchantment p /= MundaneItem = enchantmentName $ enchantment p
              | otherwise = ""

fixPN :: (Possession -> String) -> Possession -> Possession 
fixPN f p | itemName p /= "" = p
          | otherwise = setName (f p) p

getPN1 :: Possession -> String 
getPN1 p = fromMaybe "Item" . foldl mplus Nothing . map ($ p) $ itemNames
{-
getPN1 p | weapon p /= [] = head $ weapon p
         | armour p /= [] = head $ armour p
         | isJust (visArt p) = fromJust (visArt p) ++ " vis"
         | isAC p = "AC to " ++ (fromJust $ acTo p)
         | otherwise = "Item"
-}

itemNames :: [ Possession -> Maybe String ]
itemNames = [ mhead . weapon, mhead . armour, fmap ( "AC to " ++ ) . visArt, fmap ( ++ " vis" ) . acTo ]

isAC :: Possession -> Bool
isAC p = isJust $ acTo p

instance Show Possession where
    show p = name p ++ cnt
       where cnt | count p == 1 = ""
                 | otherwise = " (" ++ show (count p) ++ ")"


data LabText = Device MagicEffect | SpellText SpellRecord
    deriving ( Ord, Eq, Generic )
instance ToJSON LabText
instance FromJSON LabText 

-- | A magic effect that can be instilled in an enchanted device.
data MagicEffect = MagicEffect
           { effectName :: String
           , effectLevel :: Int
           , effectTechnique :: String
           , effectTechniqueReq :: [String]
           , effectForm :: String
           , effectFormReq :: [String]
           , effectRange :: String        -- ^ Range
           , effectDuration :: String     -- ^ Duration
           , effectTarget :: String       -- ^ Target
           , effectModifiers :: [ String ]
           , effectTrigger :: String
           , effectDesign :: String     -- ^ Level calculation
           , effectDescription :: [String]
           , effectComment :: [String]    -- ^ Freeform remarks that do not fit elsewhere
           , effectReference :: String  -- ^ Source reference
           , effectDate :: SeasonTime   -- ^ Time of investment
           }
           deriving (Show, Eq, Ord, Generic)


instance ToJSON MagicEffect
instance FromJSON MagicEffect where
    parseJSON = withObject "MagicEffect" $ \v -> MagicEffect
        <$> v .: "name" 
        <*> v .: "level" 
        <*> v .: "technique" 
        <*> v  `parseCollapsedList` "techiqueReq" 
        <*> v .: "form" 
        <*> v  `parseCollapsedList` "formReq" 
        <*> v .:? "range" .!= ""
        <*> v .:? "duration" .!= ""
        <*> v .:? "target" .!= ""
        <*> v `parseCollapsedList` "modifiers" 
        <*> v .:? "trigger"  .!= ""
        <*> v .:? "design"  .!= ""
        <*> v `parseCollapsedList` "description" 
        <*> v `parseCollapsedList` "comment" 
        <*> v .:? "reference"  .!= ""
        <*> v .:? "season" .!= NoTime
