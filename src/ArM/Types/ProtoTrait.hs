{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.ProtoTrait
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
module ArM.Types.ProtoTrait ( module ArM.Types.Trait
                      , ProtoTrait(..)
                      , TraitKey(..)
                      , advanceTraitList
                      , defaultPT
                      , spellKeyName
                      , Weapon(..)
                      , Armour(..)
                      , findTrait
                      , processChar
                      , regularXP
                      , getVF
                      ) where

import ArM.GameRules
import ArM.Helper
import ArM.Types.Trait
import ArM.Types.TraitKey
import ArM.Types.HarmObject
import ArM.Types.Aging
import ArM.Types.Lab

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe 
import Data.List
import Control.Monad

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
    { protoTrait :: TraitKey
    , spell :: Maybe String  -- ^ spell name
    , reputation :: Maybe String  -- ^ reputation contents
    , aging :: Maybe Aging        -- ^ Aging object 
    , possession :: Maybe Possession -- ^ Possesion includes weapon, vis, equipment, etc.
    , lab :: Maybe Lab            -- ^ Possesion includes weapon, vis, equipment, etc.
    , combat :: Maybe CombatOption -- ^ Possesion includes weapon, vis, equipment, etc.
    , spec :: Maybe String        -- ^ specialisation of an ability
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
    , ptComment :: Maybe String  -- ^ freeform comment
    } deriving (Eq,Generic)

-- | Default ProtoTrait object, used internally for step-by-step construction of
-- new objects.
defaultPT :: ProtoTrait
defaultPT = ProtoTrait { protoTrait = NoTrait
                             , spell = Nothing
                             , reputation = Nothing
                             , aging = Nothing
                             , possession = Nothing
                             , lab = Nothing
                             , combat = Nothing
                             , spec = Nothing
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
                             , ptComment = Nothing
                             }

parseAbilityKey :: Object -> Parser TraitKey
parseAbilityKey v = AbilityKey <$> v .:  "ability"
parseArtKey :: Object -> Parser TraitKey
parseArtKey v = artKey <$> v .:  "art"
parseVirtueKey :: Object -> Parser TraitKey
parseVirtueKey v = VFKey <$> v .:  "virtue" <*> v .:? "detail" .!= ""
parseFlawKey :: Object -> Parser TraitKey
parseFlawKey v = VFKey <$> v .:  "flaw" <*> v .:? "detail" .!= ""
parseCharKey :: Object -> Parser TraitKey
parseCharKey v = CharacteristicKey <$> v .: "characteristic" 
parsePTraitKey :: Object -> Parser TraitKey
parsePTraitKey v = PTraitKey <$> v .: "ptrait" 
parseConfidenceKey :: Object -> Parser TraitKey
parseConfidenceKey v = ConfidenceKey <$> v .: "confidence" 
parseOtherTraitKey :: Object -> Parser TraitKey
parseOtherTraitKey v = OtherTraitKey <$> v .: "other" 

parseKey :: Object -> Parser TraitKey
parseKey v = foldr mplus (pure NoTrait)
          [ (parseArtKey v), (parseAbilityKey v), (parseVirtueKey v), (parseFlawKey v)
          , (parsePTraitKey v), (parseCharKey v), (parseConfidenceKey v) 
          , (parseOtherTraitKey v) ]

instance ToJSON ProtoTrait 
instance FromJSON ProtoTrait where
    parseJSON = withObject "ProtoTrait" $ \v -> ProtoTrait
        <$> parseKey v
        <*> v .:?  "spell"
        <*> v .:?  "reputation"
        <*> v .:?  "aging"
        <*> v .:?  "possession"
        <*> v .:?  "lab"
        <*> v .:?  "combat"
        <*> v .:?  "spec"
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

instance StoryObject ProtoTrait where
   name = show . traitKey
   comment = fromMaybe [] . fmap (:[]) . ptComment
instance Show ProtoTrait  where
   show p = showPT k p ++ cms
      where cmt = (ptComment p)
            cms | isNothing cmt = ""
                | otherwise = " (" ++ fromJust cmt ++ ")"
            k = protoTrait p

-- | Auxiliary for show
showPT :: TraitKey -> ProtoTrait -> String
showPT (AbilityKey x) p =
           "Ability: " ++ x  ++ showSpec p
           ++ showXP p
           ++ showBonusScore p ++ "; " ++ showMult p
showPT art@(ArtKey _) p =
           "Art: " ++ show art ++ showXP p
           ++ showBonusScore p ++ "; " ++ showMult p
showPT (VFKey vfn d) p =
              "Virtue/Flaw: " ++ vfn ++ ds ++ " ("
              ++ show ( fromMaybe 0 (cost p) ) 
              ++ mul (multiplicity p)
              ++ ")"
        where ds | d == "" = d
                 | otherwise = ':':' ':d
              mul Nothing = ""
              mul (Just x) = " x" ++ show x
showPT (CharacteristicKey cn) p = "Characteristic: " ++ cn  ++
           " " ++ show ( fromMaybe 0 (score p) ) ++ showAging p 
showPT (PTraitKey cn) p = "Personality Trait: " ++ cn
                     ++ " " ++ maybeShow (score p)
showPT (ConfidenceKey cn) p = 
              cn ++ ": " ++ show (fromMaybe 0 (score p)) ++ " (" ++
              show ( fromMaybe 0 (points p) ) ++ ")"
showPT (OtherTraitKey cn) p = cn ++ " " ++ show ( fromMaybe 0 ( points p ) )
showPT _ p 
       | spell p /= Nothing =
              "Spell: " ++ fromJust (spell p) ++ showXP p
                    ++ showMastery (mastery p) ++ showMult p
                    ++ showFlawless p
       | reputation p /= Nothing = 
              "Reputation: " ++ fromJust (reputation p) ++
              " [" ++ (fromMaybe "--" $ locale p) ++ "]" ++ showXP p
       | possession p /= Nothing = "Possession: " ++ show (fromJust $ possession p)
       | lab p /= Nothing = "Lab: " ++ show (name $ fromJust $ lab p)
       | combat p /= Nothing = show (fromJust $ combat p)
       | aging p /= Nothing = show (fromJust $ aging p)
       | otherwise  = ttrace $ "No Trait for this ProtoTrait (showPT) " ++ (show $ protoTrait p)


instance Ord ProtoTrait where
     compare x y = compare (traitKey x) (traitKey y)

showAging :: ProtoTrait -> String
showAging p | Nothing == aging p = ""
            | otherwise = " (" ++ (ishow $ agingPts p) ++ " aging points)"
    where ishow Nothing = "-"
          ishow (Just x) = show x

instance TraitClass ProtoTrait where
   traitKey p
       | NoTrait /= (protoTrait p) = protoTrait p
       | spell p /= Nothing = SpellKey (fote $ fromMaybe "TeFo" $ tefo p)
                           (fromMaybe 0 $ level p ) ( fromJust $ spell p ) 
       | reputation p /= Nothing = ReputationKey (fromJust (reputation p)) (fromMaybe "" (locale p))
       | possession p /= Nothing = traitKey $ fromJust $ possession p
       | lab p /= Nothing = traitKey $ fromJust $ lab p
       | combat p /= Nothing = traitKey $ fromJust $ combat p
       | aging p /= Nothing = AgeKey
       | otherwise  = trace (show p) $ error "No Trait for this ProtoTrait (traitKey)"

   toTrait p = fromJust $ foldr mplus err [ f p | f <- computeList ]
            where err = error "No Trait for this ProtoTrait (toTrait)" 
   getTrait _ = Nothing

computeList :: [ ProtoTrait -> Maybe Trait ]
computeList = [ \ p -> computeTrait' (traitKey p) p
              , fmap CombatOptionTrait . combat
              , fmap EstateTrait . lab
              , fmap PossessionTrait . possession
              , fmap AgeTrait . computeTrait
              , fmap SpellTrait . computeTrait
              , fmap ReputationTrait . computeTrait
              ]


computeTrait' :: TraitKey -> ProtoTrait -> Maybe Trait
computeTrait' NoTrait _ = Nothing
computeTrait' (AbilityKey x) p = Just $ AbilityTrait $
           Ability { abilityName = x
                , speciality = spec p
                , abilityXP = fromMaybe 0 (xp p)
                , abilityScore = s
                , abilityExcessXP = y
                , abilityBonus = fromMaybe 0 $ bonusScore p
                , abilityMultiplier = fromMaybe 1.0 $ multiplyXP p
                }
      where (s,y) = getAbilityScore (xp p)
computeTrait' vf@(VFKey _ _) p = fmap VFTrait $ computeVF vf p
computeTrait' (CharacteristicKey x) p = Just $ CharacteristicTrait $ Characteristic
                { characteristicName = x
                , charScore = fromMaybe 0 (score p) + fromMaybe 0 (bonusScore p)
                , agingPoints = fromMaybe 0 (agingPts p)
                , charBonusList = charBonuses p }
computeTrait' (ArtKey nam) p = Just $ ArtTrait $
                Art { artName = artLongName nam
                    , artXP = x
                    , artScore = s
                    , artExcessXP = y
                    , artBonus = fromMaybe 0 $ bonusScore p
                    , artMultiplier = fromMaybe 1.0 $ multiplyXP p
                    }
     where   y = x - pyramidScore s
             s = scoreFromXP x
             x = fromMaybe 0 (xp p) 
computeTrait' (PTraitKey nam) p = Just $ PTraitTrait $ PTrait { ptraitName = nam
                           , pscore = fromMaybe 0 (score p) }
computeTrait' (ConfidenceKey nam) p = Just $ ConfidenceTrait $ Confidence { cname = nam
                           , cscore = fromMaybe 0 (score p) 
                           , cpoints = fromMaybe 0 (points p)
                           }
computeTrait' (OtherTraitKey n) p = Just $ OtherTraitTrait
                $ updateOther xpts $ OtherTrait { trait = n
                             , otherScore = fromMaybe 0 (score p) 
                             , otherExcess = 0 }
         where xpts = fromMaybe 0 (points p) 
computeTrait' _ _ = Nothing

computeVF :: TraitKey -> ProtoTrait -> Maybe VF
computeVF (VFKey x d) p = Just $ VF
                    { vfname = x, vfcost = fromMaybe 0 (cost p), vfDetail = d
                    , vfAppliesTo = Nothing
                    , vfMultiplicity = fromMaybe 1 $ multiplicity p
                    , vfComment = fromMaybe "" $ ptComment p }
computeVF _ _ = Nothing

-- * Advancement - the TraitType class
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
    computeTrait _ = error "Characteristic computeTrait not implemented"
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
    computeTrait _ = error "computeTrait VF not implemented"
    advanceTrait a x = x { vfMultiplicity = vfMultiplicity x + (fromMaybe 1 $ multiplicity a) }
instance TraitType Ability where
    computeTrait _ = error "computeTrait Ability not implemented"
    advanceTrait a x = 
          updateBonus (bonusScore a) $ um (multiplyXP a) $
          updateAbilitySpec (spec a) $ updateAbilityXP lim y x
      where y = calcXP m (abilityExcessXP x) (xp a) 
            m = abilityMultiplier x
            um Nothing ab = ab 
            um abm ab = ab { abilityMultiplier = fromMaybe 1.0 abm }
            lim = levelCap a
instance TraitType Art where
    computeTrait _ = error "computeTrait Art not implemented"
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
                      , spellTComment = fromMaybe "" $ ptComment p
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
    computeTrait _ = error "computeTrait personality trait not implemented"
    advanceTrait _ x = trace "Warning! Advancement not implemented for personality traits"  x
instance TraitType Confidence where
    computeTrait _ = error "computeTrait Confidence not implemented"
    advanceTrait a = updateCScore (score a) . updateCPoints (points a) 
       where updateCScore Nothing x = x
             updateCScore (Just y) x = x { cscore = y }
             updateCPoints Nothing x = x
             updateCPoints (Just y) x = x { cpoints = y + cpoints x }

instance TraitType OtherTrait where
    computeTrait _ = error "computeTrait OtherTrait not implemented"
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
    advanceTrait a (PossessionTrait x) =  toTrait $ advanceTrait a x
    advanceTrait a (EstateTrait x) =  toTrait $ advanceTrait a x
    advanceTrait a (CombatOptionTrait x) =  toTrait $ advanceTrait a x
    advanceTrait a (AgeTrait x) = AgeTrait $ advanceTrait a x
    computeTrait = Just . toTrait

instance TraitType Possession where
    advanceTrait p x = addCount x ( fromMaybe 1 m )
        where  m = fmap count $ possession  p
    computeTrait = possession
instance TraitType Lab where
    advanceTrait p x = fromMaybe x $ lab p
    computeTrait = lab

instance TraitType CombatOption where
    advanceTrait p _ = fromJust $ combat p
    computeTrait = combat 
instance TraitType Age where
    advanceTrait p = advanceAge ag
          where ag = fromJust $ aging p
    computeTrait p | isNothing (aging p) = Nothing
                   | otherwise = fmap toAge (aging p)

-- |
-- = Advancement

-- | Apply a list of ProtoTrait advancements to a list of Traits.
--
-- advance a list of traits.  This is the main function used by other
-- modules when characters are advanced.
advanceTraitList :: [ ProtoTrait ] -> [ Trait ] -> [ Trait ]
advanceTraitList x = filter (not . isNone) .  advanceTraitList' x

-- This is a helper for `advanceTraitList`.  It advances the list
-- of traits, but leaving traits with zero count in.
advanceTraitList' :: [ ProtoTrait ] -> [ Trait ] -> [ Trait ]
advanceTraitList' [] ys = ys
advanceTraitList' (x:xs) [] = advanceTraitList xs [toTrait x]
advanceTraitList' (x:xs) (y:ys) 
    | x <: y = advanceTraitList xs (toTrait x:y:ys)
    | y <: x = y:advanceTraitList (x:xs) ys
    | otherwise = advanceTraitList xs (advanceTrait x y:ys)

-- ** Auxiliary update functions

-- | Change the speciality of an Ability
updateAbilitySpec :: Maybe String -> Ability -> Ability
updateAbilitySpec Nothing a = a
updateAbilitySpec (Just x) a = a { speciality = Just x }

-- | Add XP to an Ability and update the score if required.
updateAbilityXP :: Maybe Int -> XPType -> Ability -> Ability
updateAbilityXP lim x ab
    | isJust lim && fromJust lim <= abilityScore ab = ab
    | x < tr = ab { abilityExcessXP = x }
    | otherwise = updateAbilityXP lim (x-tr) 
                $ ab { abilityScore = sc+1, abilityExcessXP = 0 }
    where sc = abilityScore ab
          tr = fromIntegral (sc+1)*5

-- | Add XP to a reputation and update the score if required.
updateRepXP :: XPType -> Reputation -> Reputation
updateRepXP x ab | x < tr = ab { repExcessXP = x }
                 | otherwise = updateRepXP (x-tr) $ ab { repScore = sc+1 }
    where sc = repScore ab
          tr = fromIntegral $ (sc+1)*5

-- | Add XP to an art and update the score if required.
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

-- ** Postprocessing of traits

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
          where x = head $ charBonusList c
                xs = tail $ charBonusList c
                sc | fst x < 0 = max (charScore c + snd x) (fst x)
                   | otherwise = min (charScore c + snd x) (fst x)

-- ** Convenience Functions

-- | Count regular XP (excluding reputation) from a ProtoTrait
regularXP :: ProtoTrait -> XPType
regularXP p = g (protoTrait p) 
     where g (AbilityKey _) = x
           g (ArtKey _) = x
           g (SpellKey _ _ _) = x
           g _ = x0
           x0 | isNothing (spell p) = 0
              | otherwise = x
           x = fromMaybe 0 $ xp p

-- | Get the virtues and flaws from a list of ProtoTrait objects, and convert them to
-- VF objects
getVF :: [ ProtoTrait ] -> [ VF ]
getVF = filterNothing . map ( \ x -> computeVF (protoTrait x) x )
