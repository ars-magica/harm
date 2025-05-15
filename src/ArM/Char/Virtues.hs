-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Char.Validation
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Code depending on specific virtues and flaws.
--
-- This module should gather all mechanics derived from virtues and
-- flaws, even if the resulting functions are used differently in
-- different modules.  By keeping virtue definitions together, it
-- is easier to see if all known virtues and flaws have have been
-- considered.
--
-----------------------------------------------------------------------------

module ArM.Char.Virtues (
                        -- * Trait inference
                        inferTraits
                        -- * Bonus XP in-game
                        , vfBonusSQ
                        -- * CharGen specific code
                        , laterLifeSQ
                        , getCharAllowance
                        , inferConfidence
                        , appSQ
                        ) where

import ArM.Types.Advancement
import ArM.Types.ProtoTrait
import ArM.Types.Aging
import ArM.Helper
import ArM.GameRules

import qualified Data.Map as Map
import Data.Maybe

-- import ArM.Debug.Trace

-- |
-- = Infer traits
--
-- The following functions are used to infer additional traits from virtues
-- and flaws.  This includes both affinities and puissant, which add traits
-- giving bonuses to regular abilities, and virtues which grant supernatural
-- abilities.

vl2 :: [ ( String, VF -> [ ProtoTrait ] ) ]
vl2 = [ ( "Puissant (art)",
         \ x -> [ defaultPT { protoTrait = ArtKey $ vfDetail x, bonusScore = Just 3 } ] )
     , ( "Puissant (ability)",
              \ x -> [ defaultPT { protoTrait = AbilityKey $ vfDetail x, bonusScore = Just 2 } ] )
     , ( "Affinity with (art)",
              \ x -> [ defaultPT { protoTrait = ArtKey $ vfDetail x, multiplyXP = Just 1.5 } ] )
     , ( "Affinity with (ability)",
              \ x -> [ defaultPT { protoTrait = AbilityKey $ vfDetail x, multiplyXP = Just 1.5 } ] )
     , ( "Strong Faerie Blood",
              \ _ -> [ defaultPT { aging = Just $ defaultAging { agingLimit = Just 50, agingBonus = Just 3 } } 
                     , defaultPT { protoTrait = AbilityKey $ "Second Sight", xp = Just 5 } 
                     , defaultPT { protoTrait = VFKey "Second Sight" "", cost = Just 0
                                 , ptComment = Just "from Strong Faerie Blood" } 
                     ] )
     , ( "Faerie Blood",
              \ _ -> [ defaultPT { aging = Just $ defaultAging { agingBonus = Just 1 } } ] )
     , ( "Great Characteristic",
              \ x -> [ defaultPT { protoTrait = CharacteristicKey $ vfDetail x
                               , charBonuses = [(5,vfMultiplicity x)]  } ] )
     , ( "Poor Characteristic",
              \ x -> [ defaultPT { protoTrait = CharacteristicKey $ vfDetail x
                               , charBonuses = [(-5,0-vfMultiplicity x)]  } ] )
     ]


vl1 :: [ ( String, VF -> [ ProtoTrait ] ) ]
vl1 = [ (ab, \ _ -> [ defaultPT { protoTrait = AbilityKey $ ab, xp = Just 5 } ] ) | ab <- snab ]

vl3 :: [ ( String, VF -> Trait ) ]
vl3 = [ ("Self-Confidence", \ _ -> confTrait 2 5 )
      , ("Low Self-Esteem", \ _ -> confTrait 0 0 )
      ]

snab :: [ String ]
snab = [ "Second Sight", "Enchanting Music"
       , "Dowsing"
       , "Magic Sensitivity", "Animal Ken"
       , "Wilderness Sense"
       , "Sense Holiness and Unholiness"
       , "Entrancement", "Premonitions"
       , "Heartbeast"
       , "Shapeshifter" ]

virtueMap :: Map.Map String ( VF -> [ ProtoTrait ] ) 
virtueMap = Map.fromList $ vl1 ++ vl2

-- |
-- = Confidence

confTrait :: Int -> Int -> Trait
confTrait x y = ConfidenceTrait $ Confidence { cname = "Confidence", cscore = x, cpoints = y } 

-- | Get the starting confidence trait, based on the given list of
-- virtues and flaws.
inferConfidence :: [VF] -> Trait
inferConfidence vfs | rs == [] = confTrait 1 3
                    | otherwise =  head rs
    where vf = [ Map.lookup (vfname x) confMap | x <- vfs ]
          app Nothing _ = Nothing
          app (Just f) x = Just $ f x
          rs = filterNothing [ app g x | (g,x) <- zip vf vfs ]

-- | Helper for `inferConfidence`
confMap :: Map.Map String ( VF -> Trait ) 
confMap = Map.fromList $ vl3

-- | Add ProtoTrait objects infered by current virtues and flaws
inferTraits :: [VF] -> [ProtoTrait]
inferTraits vfs = sortTraits $ foldl (++) [] rs
    where vf = [ Map.lookup (vfname x) virtueMap | x <- vfs ]
          app Nothing _ = Nothing
          app (Just f) x = Just $ f x
          rs = filterNothing [ app g x | (g,x) <- zip vf vfs ]

-- | Get pre-game XP from given virtue
llLookup:: String -> (XPType,XPType)
llLookup "Warrior" = (50,0) 
llLookup "Wealthy" = (0,20) 
llLookup "Poor" = (0,10) 
llLookup _  = (0,0) 

-- | Get XP for later life, both fixed allowances from virtues
-- and the allowance per year.
laterLifeXP :: [ VF ] -> (XPType,XPType)
laterLifeXP vfs = laterLifeXP' vfs (0,15)
laterLifeXP' :: [ VF ] -> (XPType,XPType) -> (XPType,XPType)
laterLifeXP' [] (x,y) = (x,y)
laterLifeXP' (vf:vfs) (x,y) = laterLifeXP' vfs $ (x'+x,f y y') 
         where (x',y') = llLookup $ vfname vf
               f 0 z = z
               f z _ = z

laterLifeSQ' :: AugmentedAdvancement -> (XPType,XPType) -> XPType
laterLifeSQ' ad (x,y) = t
   where t | isJust (sourceQuality ad) = fromJust (sourceQuality ad)
           | isJust (years ad) = x+y*yy
           | otherwise = x
         yy = fromIntegral $ fromMaybe 0 (years ad)

-- | Get XP total for Later Life
laterLifeSQ :: [VF] -> AugmentedAdvancement -> XPType
laterLifeSQ vfs ad = laterLifeSQ' ad $ laterLifeXP vfs

-- | Get XP and spell level total for Apprenticeship
appSQ :: [VF] -> (XPType,Int)
appSQ []  = (240,120) 
appSQ (x:xs) | vfname x == "Weak Parens" = (180,90) 
             | vfname x == "Skilled Parens" = (300,150) 
             | otherwise = appSQ xs


-- | Get Bonus SQ/XP for in-game advancement types, dependent
-- on mode of study.
vfBonusSQ :: AdvancementLike a => [VF] -> a -> [BonusSQ]
vfBonusSQ vs a = filterNothing $ map (vfBonusSQ' a) vs

vfBonusSQ' :: AdvancementLike a => a ->  VF -> Maybe BonusSQ
vfBonusSQ' m vf = g (vfname vf) (mode m)
    where  f "Apt Student" Taught = 5 :: XPType
           f "Apt Student" Trained = 5
           f "Poor Student" Taught = -3
           f "Poor Student" Reading = -3
           f "Book Learner" Reading = 3
           f "Independent Study" Practice = 2
           f "Independent Study" Adventure = 3
           f "Free Study" VisStudy = 3
           f _ _ = 0
           g x y | 0 == f x y = Nothing
                 | otherwise = Just $ BonusSQ { sourceBonus = f x y, bonusSource = x }


-- | Get the point allowance for characteristics purchase,
-- which is seven points adjusted by virtues and flaws.
getCharAllowance :: [ VF ] -> Int
getCharAllowance = (+7) . sum . map ( chLookup . vfname )
     where chLookup "Improved Characteristics" = 3
           chLookup "Weak Characteristics" = -3
           chLookup _  = 0

