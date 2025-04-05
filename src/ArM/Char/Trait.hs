{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Trait
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
-- 
-- Description :  Types to Traits and advancement of Traits
--
-- Traits are represented as `ProtoTrait` objects in the design and
-- advancement. A `ProtoTrait` can represent either a new trait or a
-- change to an existing trait.
--
-- Processing advancements, the `ProtoTrait` is converted to a `Trait` 
-- object, representing the current state of the trait.
--
-- There are individual types for each kind of trait, e.g. virtue/flaw,
-- ability, spell, art, etc.   Conversion between a `Trait` object and
-- a more specific type, like `Art`, is done by the (polymorphic)
-- `toTrait` and `getTrait` functions.
--
-----------------------------------------------------------------------------
module ArM.Char.Trait ( module ArM.Char.Types.Trait
                      , ProtoTrait(..)
                      , TraitKey(..)
                      , TraitType(..)
                      , advanceTraitList
                      , defaultPT
                      , spellKeyName
                      , Aging(..)
                      , defaultAging
                      , Weapon(..)
                      , Armour(..)
                      , findTrait
                      , processChar
                      ) where

import ArM.GameRules
import ArM.Helper
import ArM.Char.Types.Trait

import GHC.Generics
import Data.Aeson
import Data.Maybe 
import Data.List

import ArM.Debug.Trace
import ArM.DB.Weapon

-- | 
-- = Convenience functions

-- | Get the spell name from a TraitKey object
spellKeyName :: TraitKey -> String
spellKeyName ( SpellKey _ _ n ) = n
spellKeyName _ = "Error!"


-- | Find a trait, given by a key, from a list of Trait objects.
findTrait :: (TraitClass a) => TraitKey -> [a] -> Maybe a
findTrait k = find ( (k==) . traitKey )

-- | 
-- = ProtoTrait

-- | A `ProtoTrait` represents a new trait or an advancement of an existing trait
-- as represented in the JSON input.  
-- Most fields are used only for certain types of traits and are Nothing for other
-- types.  This is the case, in particular, for the first quite a few fields which
-- give the name of the trait of the relevant type only. 
data ProtoTrait = ProtoTrait
    { ability :: Maybe String  -- ^ ability name 
    , virtue :: Maybe String   -- ^ virtue name
    , flaw :: Maybe String     -- ^ flaw name
    , characteristic :: Maybe String  -- ^ characteristic name
    , art :: Maybe String  -- ^ art name
    , spell :: Maybe String  -- ^ spell name
    , ptrait :: Maybe String  -- ^ personality trait name
    , confidence :: Maybe String  -- ^ confidence, true faith, or similar
    , reputation :: Maybe String  -- ^ reputation contents
    , other :: Maybe String       -- ^ other trait, e.g. warping or decrepitude
    , strait :: Maybe String      -- ^ special trait, like Longevity Potion
    , aging :: Maybe Aging        -- ^ Aging object 
    , possession :: Maybe Possession -- ^ Possesion includes weapon, vis, equipment, etc.
    , combat :: Maybe CombatOption -- ^ Possesion includes weapon, vis, equipment, etc.
    , protoTraitKey :: TraitKey
    , spec :: Maybe String        -- ^ specialisation of an ability
    , detail :: Maybe String      -- ^ detail (options) for a virtue or flaw
    , appliesTo :: Maybe TraitKey  -- ^ not used (intended for virtues/flaws applying to another trait)
    , levelCap :: Maybe Int    -- ^ cap on advancement
    , level :: Maybe Int       -- ^ level of a spell
    , tefo :: Maybe String     -- ^ technique/form of a spell
    , locale :: Maybe String   -- ^ locale/domain of a reputation
    , mastery :: Maybe [ String ]   -- ^ mastery options for a spell
    , flawless :: Maybe Bool   -- ^ for a spell, if flawless magic applies
    , score :: Maybe Int       -- ^ new score to replace the old one
    , bonusScore :: Maybe Int  -- ^ bonus from puissant; should also be used on initial 
                               -- characteristics for the bonus from Strong Faerie Blood
                               -- or Great/Poor Characteristic to keep it separate from
                               -- the levels bought from points.
    , multiplyXP :: Maybe Float  -- ^ XP multiplier from affinities and similar
    , cost :: Maybe Int          -- ^ cost of a virtue or flaw
    , points :: Maybe Int        -- ^ points for confidence/true faith/etc (additive)
    , xp :: Maybe XPType         -- ^ XP to be added to the trait
    , agingPts :: Maybe Int      -- ^ aging points for characteristicds (additive)
    , charBonuses :: [(Int,Int)] -- ^ bonuses from virtues to apply to a characteristic
    , multiplicity :: Maybe Int  -- ^ number of types a virtue/flaw is taken;
                                 -- could be negative to remove an existing, but
                                 -- this is not yet implemented
    , comment :: Maybe String    -- ^ freeform comment
    } deriving (Eq,Generic)

-- | Default ProtoTrait object, used internally for step-by-step construction of
-- new objects.
defaultPT :: ProtoTrait
defaultPT = ProtoTrait { ability = Nothing
                             , virtue = Nothing
                             , flaw = Nothing
                             , characteristic = Nothing
                             , art = Nothing
                             , spell = Nothing
                             , ptrait = Nothing
                             , confidence = Nothing
                             , reputation = Nothing
                             , other = Nothing
                             , strait = Nothing
                             , aging = Nothing
                             , possession = Nothing
                             , combat = Nothing
                             , protoTraitKey = NoTrait
                             , spec = Nothing
                             , detail = Nothing
                             , appliesTo = Nothing
                             , levelCap = Nothing
                             , level = Nothing
                             , tefo = Nothing
                             , locale = Nothing
                             , mastery = Nothing
                             , flawless = Nothing
                             , score = Nothing
                             , bonusScore = Nothing
                             , multiplyXP = Nothing
                             , cost = Nothing
                             , points = Nothing
                             , xp = Nothing
                             , agingPts = Nothing
                             , charBonuses = []
                             , multiplicity = Nothing
                             , comment = Nothing
                             }

instance ToJSON ProtoTrait 
instance FromJSON ProtoTrait where
    parseJSON = withObject "ProtoTrait" $ \v -> ProtoTrait
        <$> v .:?  "ability"
        <*> v .:?  "virtue"
        <*> v .:?  "flaw"
        <*> v .:?  "characteristic"
        <*> v .:?  "art"
        <*> v .:?  "spell"
        <*> v .:?  "ptrait"
        <*> v .:?  "confidence"
        <*> v .:?  "reputation"
        <*> v .:?  "other"
        <*> v .:?  "specialTrait"
        <*> v .:?  "aging"
        <*> v .:?  "possession"
        <*> v .:?  "combat"
        <*> return NoTrait
        <*> v .:?  "spec"
        <*> v .:?  "detail"
        <*> v .:?  "appliesTo"
        <*> v .:?  "levelCap"
        <*> v .:?  "level"
        <*> v .:?  "tefo"
        <*> v .:?  "locale"
        <*> v .:?  "mastery"
        <*> v .:?  "flawless"
        <*> v .:?  "score"
        <*> v .:?  "bonusScore"
        <*> v .:?  "multiplyXP"
        <*> v .:?  "cost"
        <*> v .:?  "points"
        <*> v .:?  "xp"
        <*> v .:?  "agingPts"
        <*> v .:?  "charBonus"  .!= []
        <*> v .:?  "multiplicity"
        <*> v .:?  "comment"

showBonusScore :: ProtoTrait -> String
showBonusScore pt | isNothing b = ""
                  | otherwise = " Bonus " ++ showSigned (fromJust b)
   where b = bonusScore pt
showMult :: ProtoTrait -> String
showMult pt | isNothing b = ""
            | otherwise = " x" ++ show (fromJust b) ++ "xp"
   where b = multiplyXP pt
showFlawless :: ProtoTrait -> String
showFlawless pt | Just True == flawless pt = " with flawless magic"
                | otherwise = ""
showSpec :: ProtoTrait -> String
showSpec pt | isNothing sp = ""
            | otherwise = " [" ++ fromJust sp ++ "]"
   where sp = spec pt
showXP :: ProtoTrait -> String
showXP p = " " ++ showNum ( fromMaybe 0 (xp p) ) ++ "xp"

showMastery :: Maybe [String] -> String
showMastery Nothing = ""
showMastery (Just []) = ""
showMastery (Just (x:xs)) = ' ':(foldl (++) x $ map (", "++) xs)

instance Show ProtoTrait  where
   show p = showPT p ++ cms
      where cmt = (comment p)
            cms | isNothing cmt = ""
                | otherwise = " (" ++ fromJust cmt ++ ")"

-- | Auxiliary for show
showPT :: ProtoTrait -> String
showPT p 
       | ability p /= Nothing = 
           "Ability: " ++ fromJust ( ability p )  ++ showSpec p
           ++ showXP p
           ++ showBonusScore p ++ "; " ++ showMult p
       | characteristic p /= Nothing =
           "Characteristic: " ++ fromJust ( characteristic p )  ++
           " " ++ show ( fromMaybe 0 (score p) ) ++ showAging p 
       | art p /= Nothing = 
           "Art: " ++ fromJust ( art p ) ++ showXP p
           ++ showBonusScore p ++ "; " ++ showMult p
       | spell p /= Nothing =
              "Spell: " ++ fromJust (spell p) ++ showXP p
                    ++ showMastery (mastery p) ++ showMult p
                    ++ showFlawless p
       | ptrait p /= Nothing = 
              "Personality Trait: " ++ fromJust (ptrait p)
                     ++ " " ++ maybeShow (score p)
       | reputation p /= Nothing = 
              "Reputation: " ++ fromJust (reputation p) ++
              " [" ++ (fromMaybe "--" $ locale p) ++ "]" ++ showXP p
       | virtue p /= Nothing = 
              "Virtue: " ++ fromJust (virtue p) ++ " ("
              ++ show ( fromMaybe 0 (cost p) ) 
              ++ mul (multiplicity p)
              ++ ")"
       | flaw p /= Nothing = 
              "Flaw: " ++ fromJust (flaw p) ++ " ("
              ++ mul (multiplicity p)
              ++ show ( fromMaybe 0 (cost p) ) ++ ")"
       | confidence p /= Nothing = 
              fromMaybe "Confidence" (confidence p) ++ ": " ++ show (fromMaybe 0 (score p)) ++ " (" ++
              show ( fromMaybe 0 (points p) ) ++ ")"
       | possession p /= Nothing = "Possession: " ++ show (fromJust $ possession p)
       | combat p /= Nothing = show (fromJust $ combat p)
       | aging p /= Nothing = show (fromJust $ aging p)
       | other p /= Nothing = 
               fromJust (other p) ++ " " ++ show ( fromMaybe 0 ( points p ) )
       | strait p /= Nothing = fromJust (strait p) 
       | otherwise  = error $ "No Trait for this ProtoTrait" 
     where mul Nothing = ""
           mul (Just x) = " x" ++ show x


instance Ord ProtoTrait where
     compare x y = compare (traitKey x) (traitKey y)

showAging :: ProtoTrait -> String
showAging p | Nothing == aging p = ""
            | otherwise = " (" ++ (ishow $ agingPts p) ++ " aging points)"
    where ishow Nothing = "-"
          ishow (Just x) = show x

instance TraitClass ProtoTrait where
   traitKey p
       | ability p /= Nothing = AbilityKey $ fromJust $ ability p 
       | characteristic p /= Nothing = CharacteristicKey $ fromJust $ characteristic p 
       | art p /= Nothing = ArtKey $ take 2 $ fromJust $ art p 
       | spell p /= Nothing = SpellKey (fote $ fromMaybe "TeFo" $ tefo p)
                           (fromMaybe 0 $ level p ) ( fromJust $ spell p ) 
       | ptrait p /= Nothing = PTraitKey $ fromJust $ ptrait p
       | reputation p /= Nothing = ReputationKey (fromJust (reputation p)) (fromMaybe "" (locale p))
       | virtue p /= Nothing = VFKey ( fromJust (virtue p) ) (fromMaybe "" $ detail p)
       | flaw p /= Nothing = VFKey ( fromJust (flaw p) ) (fromMaybe "" $ detail p)
       | confidence p /= Nothing = ConfidenceKey $ fromMaybe "Confidence" $ confidence p
       | other p /= Nothing = OtherTraitKey $ fromJust $ other p
       | possession p /= Nothing = traitKey $ fromJust $ possession p
       | combat p /= Nothing = traitKey $ fromJust $ combat p
       | aging p /= Nothing = AgeKey
       | strait p /= Nothing = trace (show p) $ error "No strait TraitKey"
       | otherwise  = trace (show p) $ error "No Trait for this ProtoTrait" 

   toTrait p 
      | ability p /= Nothing = AbilityTrait $ fromJust $ computeTrait p
      | characteristic p /= Nothing = CharacteristicTrait $  fromJust $ computeTrait p
      | art p /= Nothing = ArtTrait $ fromJust $ computeTrait p
      | spell p /= Nothing = SpellTrait $ fromJust $ computeTrait p
      | ptrait p /= Nothing = PTraitTrait $
           PTrait { ptraitName = fromJust (ptrait p)
                  , pscore = fromMaybe 0 (score p)
                  } 
      | reputation p /= Nothing = ReputationTrait $ fromJust $ computeTrait p
      | virtue p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | flaw p /= Nothing = VFTrait $ fromJust $ computeTrait p
      | confidence p /= Nothing = ConfidenceTrait $ 
           Confidence { cname = fromMaybe "Confidence" (confidence p)
                      , cscore = fromMaybe 0 (score p)
                      , cpoints = fromMaybe 0 (points p) }
      | other p /= Nothing = OtherTraitTrait $ fromJust $ computeTrait p
      | possession p /= Nothing = PossessionTrait $ fromJust $ computeTrait p
      | combat p /= Nothing = CombatOptionTrait $ fromJust $ computeTrait p
      | aging p /= Nothing = AgeTrait $ fromJust $ computeTrait p
      | otherwise  = error "No Trait for this ProtoTrait" 
   getTrait _ = Nothing

-- |
-- == Aging

defaultAging :: Aging
defaultAging = Aging
    { addYears       = Nothing
    , deltaYounger   = Nothing
    , agingRollDie   = Nothing
    , agingRoll      = Nothing
    , longevity      = Nothing
    , agingLimit     = Nothing
    , agingBonus     = Nothing
    , agingComment   = Nothing
    }
data Aging = Aging
    { addYears       :: Maybe Int
    , deltaYounger   :: Maybe Int   
        -- ^ Should be 1 when age changes and apparent age does not, otherwise 0
    , agingRollDie   :: Maybe Int    -- ^ aging roll die result
    , agingRoll      :: Maybe Int    -- ^ aging roll total
    , longevity      :: Maybe Int    -- ^ score of new longevity ritual
    , agingLimit     :: Maybe Int    -- ^ freeform comment
    , agingBonus     :: Maybe Int    -- ^ Bonus to aging rolls (excluding LR)
    , agingComment   :: Maybe String -- ^ age when aging rolls are required
    } deriving (Ord,Eq,Generic)
instance ToJSON Aging
instance FromJSON Aging 

instance TraitClass Aging where
    traitKey _ = AgeKey
    toTrait p = AgeTrait $ fromJust $ computeTrait $ defaultPT { aging = Just p } 
    getTrait _ = Nothing
instance Show Aging where
    show x = "Aging " ++ y ++ lr ++ roll ++ lim ++ b ++ fromMaybe "" (agingComment x)
       where y | isNothing (addYears x) = ""
               | otherwise = show yr ++ " years; apparent " 
                    ++ show (yr-del) ++ " years."
             yr = fromJust $ addYears x
             del = fromMaybe 0 $ deltaYounger x
             lr | isNothing (longevity x) = ""
               | otherwise = " LR " ++ show (fromJust $ longevity x) ++ "; "
             lim | isNothing (agingLimit x) = ""
                | otherwise = "(limit " ++ show (fromJust $ agingLimit x) ++ ") "
             b | isNothing (agingBonus x) = ""
                | otherwise = "(bonus " ++ show (fromJust $ agingBonus x) ++ ") "
             roll | isNothing (agingRoll x) = " No roll. "
                | otherwise = "Rolled " ++ show (fromJust $ agingRoll x) ++ " ("
                           ++ show (fromMaybe (-1) $ agingRollDie x) ++ ") "


-- |
-- = Advancement - the TraitType class
--
-- Advancement of traits is based `ProtoTrait` objects representing
-- changes in objects.  The `TraitType` class provides two functions.
-- Firstly `advanceTrait` applies a `ProtoTrait` to a `Trait` to advance
-- it to a new `Trait`.  Secondly, `computeTrait` creates a previously
-- non-existing trait from a `ProtoTrait`.
--
-- These functions are implemented for each type representing a kind of 
-- trait.

-- | The `TraitType` class provides the methods to advance traits
-- using `ProtoTrait` objects.
class TraitType t where

    -- | Convert a ProtoTrait (advancement) to a new trait object.
    computeTrait :: ProtoTrait -> Maybe t

    -- | Advance a trait using changes defined by a ProtoTrait.
    advanceTrait :: ProtoTrait -> t -> t
    advanceTrait _ x = x

instance TraitType Characteristic where
    computeTrait p
       | isNothing (characteristic p) = Nothing 
       | otherwise = Just $
          Characteristic { characteristicName = fromJust ( characteristic p ) 
                , charScore = fromMaybe 0 (score p) + fromMaybe 0 (bonusScore p)
                , agingPoints = fromMaybe 0 (agingPts p)
                , charBonusList = charBonuses p }
    advanceTrait a =  agingChar apts . newCharScore newscore . ncb (charBonuses a) 
       where newscore = score a
             apts = agingPts a
             ncb b x = x { charBonusList = b ++ charBonusList x }

-- | Add aging points to a characteristic and reduce it if necessary
agingChar  :: Maybe Int -> Characteristic -> Characteristic
agingChar  Nothing x = x
agingChar  (Just pt) x 
      | newpoints > asc = x { charScore = sc-1, agingPoints = 0 }
      | otherwise = x { agingPoints = newpoints }
    where newpoints = pt + agingPoints x
          sc = charScore x
          asc = abs sc

-- | Change the score of a characteristic.
-- This applies typically as a result of CrMe/CrCo rituals.
newCharScore  :: Maybe Int -> Characteristic -> Characteristic
newCharScore  Nothing x = x
newCharScore  (Just s) x = x { charScore = s }

instance TraitType VF where
    computeTrait p 
       | virtue p /= Nothing = Just $ vf1 { vfname = fromJust (virtue p) }
       | flaw p /= Nothing = Just $ vf1 { vfname = fromJust (flaw p) }
       | otherwise = Nothing
      where vf1 = VF { vfname = "", vfcost = fromMaybe 0 (cost p), vfDetail = fromMaybe "" $ detail p
                    , vfAppliesTo = Nothing
                    , vfMultiplicity = fromMaybe 1 $ multiplicity p
                    , vfComment = fromMaybe "" $ comment p }
    advanceTrait a x = x { vfMultiplicity = vfMultiplicity x + (fromMaybe 1 $ multiplicity a) }
instance TraitType Ability where
    computeTrait p
       | ability p == Nothing = Nothing
       | otherwise = Just $
           Ability { abilityName = fromJust ( ability p ) 
                , speciality = spec p
                , abilityXP = fromMaybe 0 (xp p)
                , abilityScore = s
                , abilityExcessXP = y
                , abilityBonus = fromMaybe 0 $ bonusScore p
                , abilityMultiplier = fromMaybe 1.0 $ multiplyXP p
                }
      where (s,y) = getAbilityScore (xp p)
    advanceTrait a x = 
          updateBonus (bonusScore a) $ um (multiplyXP a) $
          updateAbilitySpec (spec a) $ updateAbilityXP lim y x
      where y = calcXP m (abilityExcessXP x) (xp a) 
            m = abilityMultiplier x
            um Nothing ab = ab 
            um abm ab = ab { abilityMultiplier = fromMaybe 1.0 abm }
            lim = levelCap a
instance TraitType Art where
    computeTrait p
        | art p == Nothing = Nothing
        | otherwise = Just $
            Art { artName = fromJust ( art p ) 
                , artXP = x
                , artScore = s
                , artExcessXP = y
                , artBonus = fromMaybe 0 $ bonusScore p
                , artMultiplier = fromMaybe 1.0 $ multiplyXP p
                }
       where y = x - pyramidScore s
             s = scoreFromXP x
             x = fromMaybe 0 (xp p) 
    advanceTrait a x = 
          updateArtBonus (bonusScore a) $ um (multiplyXP a) $ 
          updateArtXP lim y x 
      where y = calcXP m (artExcessXP x) (xp a) 
            m = artMultiplier x
            um Nothing ab = ab 
            um abm ar = ar { artMultiplier = fromMaybe 1.0 abm }
            lim = levelCap a
instance TraitType Spell where
    computeTrait p | spell p == Nothing = Nothing
                   | otherwise =  Just sp 
          where sp = Spell { spellName = fromJust (spell p)
                      , spellLevel = fromMaybe 0 $ level p
                      , spellTeFo = fromMaybe "TeFo" $ tefo p
                      , spellXP = fromMaybe 0 (xp p)
                      , masteryScore = s
                      , masteryOptions = fromMaybe [] (mastery p)
                      , spellExcessXP = y
                      , spellMultiplier = m
                      , spellCastingScore = Nothing
                      , spellTComment = fromMaybe "" $ comment p
                      }
                (s',y) = getAbilityScore (xp p)
                fless = fromMaybe False $ flawless p
                m | fless = 2
                  | otherwise = fromMaybe 1.0 $ multiplyXP p
                s | s' > 0 = s'
                  | fless = 1
                  | otherwise = 0
    advanceTrait a x = updateSpellXP y           -- add XP and update score
                     $ updateSpellMastery ms     -- add new mastery options
                     $ um (multiplyXP a)         -- update multiplier 
                     x
      -- where y = (spellExcessXP x) + (fromMaybe 0 $ xp a)
      where y = calcXP m (spellExcessXP x) (xp a) 
            m = spellMultiplier x
            ms = fromMaybe [] $ mastery a
            um Nothing ab = ab 
            um abm ar = ar { spellMultiplier = fromMaybe 1.0 abm }
instance TraitType Reputation where
    computeTrait p
       | reputation p == Nothing = Nothing
       | otherwise = Just $
           Reputation { reputationName = fromJust (reputation p)
                      , repLocale = fromMaybe "" (locale p)
                      , repXP = fromMaybe 0 (xp p)
                      , repScore = s
                      , repExcessXP = y
                      }
      where (s,y) = getAbilityScore (xp p)
    advanceTrait a x = updateRepXP y x
      where y = (repExcessXP x) + (fromMaybe 0 $ xp a)
instance TraitType PTrait where
    computeTrait p 
       | ptrait p /= Nothing = Just $ PTrait 
                           { ptraitName = fromJust ( ptrait p )
                           , pscore = fromMaybe 0 (score p) }
       | otherwise = Nothing
    advanceTrait _ x = trace "Warning! Advancement not implemented for personality traits"  x
instance TraitType Confidence where
    computeTrait p 
       | confidence p /= Nothing = Just $ Confidence 
                           { cname = fromMaybe "Confidence" ( confidence p )
                           , cscore = fromMaybe 0 (score p) 
                           , cpoints = fromMaybe 0 (points p) 
                           }
       | otherwise = Nothing
    advanceTrait a = updateCScore (score a) . updateCPoints (points a) 
       where updateCScore Nothing x = x
             updateCScore (Just y) x = x { cscore = y }
             updateCPoints Nothing x = x
             updateCPoints (Just y) x = x { cpoints = y + cpoints x }

instance TraitType OtherTrait where
    computeTrait p | isNothing (other p) = Nothing
                   | otherwise = Just $ updateOther xpts df
       where df = OtherTrait { trait = fromJust ( other p )
                             , otherScore = fromMaybe 0 (score p) 
                             , otherExcess = 0 }
             xpts = fromMaybe 0 (points p) 
    advanceTrait a x = updateOther y x
      where y = otherExcess x + (fromMaybe 0 $ points a)

-- | Auxiliary for `TraitType` instance  `OtherTrait`
updateOther :: Int -> OtherTrait -> OtherTrait
updateOther x ab
    | x < tr = ab { otherExcess = x }
    | otherwise = updateOther (x-tr) 
                $ ab { otherScore = sc+1, otherExcess = 0 }
    where sc = otherScore ab
          tr = (sc+1)*5

instance TraitType SpecialTrait where
    computeTrait p 
       | strait p /= Nothing = Just $ SpecialTrait 
                           { specialTrait = fromJust ( strait p )
                           , specialScore = (score p) 
                           , specialComment = comment p
                           }
       | otherwise = Nothing
    advanceTrait a _ = fromJust $ computeTrait a


instance TraitType Trait where
    advanceTrait a (CharacteristicTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (AbilityTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ArtTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (SpellTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ReputationTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (VFTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (PTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (OtherTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (ConfidenceTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (SpecialTraitTrait x) = toTrait $ advanceTrait a x
    advanceTrait a (PossessionTrait x) =  toTrait $ advanceTrait a x
    advanceTrait a (CombatOptionTrait x) =  toTrait $ advanceTrait a x
    advanceTrait a (AgeTrait x) = AgeTrait $ advanceTrait a x
    computeTrait = Just . toTrait

instance TraitType Possession where
    advanceTrait p x = x { itemCount = itemCount x + (fromMaybe 1 $ multiplicity p) }
    computeTrait = possession

instance TraitType CombatOption where
    advanceTrait p _ = fromJust $ computeTrait p
    computeTrait = combat 
instance TraitType Age where
    advanceTrait p x = updateLR (longevity ag ) 
                     $ updateABonus ( agingBonus ag )
                     $ updateAge ( addYears ag )
                     $ x { apparentYounger = apparentYounger x + del }
          where ag = fromJust $ aging p
                updateLR Nothing y = y
                updateLR (Just lr) y = y { longevityRitual = lr }
                updateABonus Nothing y = y
                updateABonus (Just b) y = y { agingRollBonus = agingRollBonus y + b }
                updateAge Nothing y = y
                updateAge (Just b) y = y { ageYears = ageYears y + b }
                del = fromMaybe 0 $ deltaYounger ag


    computeTrait p
       | isNothing (aging p) = Nothing
       | otherwise =  Just $ Age { ageYears = fromMaybe 0 $ addYears ag
                , ageLimit = fromMaybe 35 $ agingLimit ag
                , apparentYounger = fromMaybe 0 $ deltaYounger ag
                , longevityRitual = (fromMaybe (-1) $ longevity ag)
                , agingRollBonus = ( fromMaybe 0 $ agingBonus ag ) 
                , ageComment = agingComment ag }
          where ag = fromJust $ aging p

-- |
-- = Advancement

-- | Apply a list of ProtoTrait advancements to a list of Traits.
--
-- This is the main function used by other modules when characters are
-- advanced.
advanceTraitList :: [ ProtoTrait ] -> [ Trait ] -> [ Trait ]
advanceTraitList [] ys = ys
advanceTraitList (x:xs) [] = advanceTraitList xs [toTrait x]
advanceTraitList (x:xs) (y:ys) 
    | x <: y = advanceTraitList xs (toTrait x:y:ys)
    | y <: x = y:advanceTraitList (x:xs) ys
    | otherwise = advanceTraitList xs (advanceTrait x y:ys)

-- |
-- == Auxiliary update functions

updateAbilitySpec :: Maybe String -> Ability -> Ability
updateAbilitySpec Nothing a = a
updateAbilitySpec (Just x) a = a { speciality = Just x }

updateAbilityXP :: Maybe Int -> XPType -> Ability -> Ability
updateAbilityXP lim x ab
    | isJust lim && fromJust lim <= abilityScore ab = ab
    | x < tr = ab { abilityExcessXP = x }
    | otherwise = updateAbilityXP lim (x-tr) 
                $ ab { abilityScore = sc+1, abilityExcessXP = 0 }
    where sc = abilityScore ab
          tr = fromIntegral (sc+1)*5

updateRepXP :: XPType -> Reputation -> Reputation
updateRepXP x ab | x < tr = ab { repExcessXP = x }
                 | otherwise = updateRepXP (x-tr) $ ab { repScore = sc+1 }
    where sc = repScore ab
          tr = fromIntegral $ (sc+1)*5

updateArtXP :: Maybe Int ->  XPType -> Art -> Art
updateArtXP lim x ab
    | isJust lim && fromJust lim <= artScore ab = ab
    | x < tr = ab { artExcessXP = x }
    | otherwise = updateArtXP lim (x-tr) $ ab { artScore = sc+1, artExcessXP = 0 }
   where sc = artScore ab
         tr = fromIntegral (sc+1)


updateSpellXP :: XPType -> Spell -> Spell
updateSpellXP x ab | x < tr = ab { spellExcessXP = x }
                   | otherwise = updateSpellXP (x-tr) $ ab { masteryScore = sc+1 }
    where sc = masteryScore ab
          tr = fromIntegral $ (sc+1)*5
updateSpellMastery :: [String] -> Spell -> Spell
updateSpellMastery ms t = t { masteryOptions = (masteryOptions t) ++ ms }


updateBonus :: Maybe Int -> Ability -> Ability
updateBonus Nothing a = a 
updateBonus (Just x) a = a { abilityBonus = x + abilityBonus a }

updateArtBonus :: Maybe Int -> Art -> Art
updateArtBonus Nothing a = a 
updateArtBonus (Just x) a = a { artBonus = x + artBonus a }

-- |
-- == Postprocessing of traits


-- | Process bonuses to characteristics, typically granted by virtues.
-- THis has to be applied after validation of the points expenditure at
-- char gen.
processChar :: Trait -> Trait 
processChar (CharacteristicTrait c) = CharacteristicTrait $ processChar' c
processChar c = c


processChar' :: Characteristic -> Characteristic 
processChar' c | charBonusList c == [] = c
     | charBonusList c == [] = c
     | otherwise = processChar'' $ c { charBonusList = sortOn f $ charBonusList c }
       where f = abs . fst
processChar'' :: Characteristic -> Characteristic 
processChar'' c | charBonusList c == [] = c
               | otherwise  = processChar'' $ c { charScore = sc, charBonusList = xs }
          where x:xs = charBonusList c
                sc | fst x < 0 = max (charScore c + snd x) (fst x)
                   | otherwise = min (charScore c + snd x) (fst x)
