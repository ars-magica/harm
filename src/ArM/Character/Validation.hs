-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Validation
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Character advancement validation.
--
-----------------------------------------------------------------------------
module ArM.Character.Validation where

import ArM.Types.Advancement
import ArM.Trait
import ArM.Processing
import ArM.GameRules
import ArM.Helper
import Data.Maybe 
import Data.List 

-- | Find the trait earning the most XP from the advancement, returning the TraitKey
primaryXPTrait :: Advancement -> Maybe TraitKey
primaryXPTrait = fmap traitKey . primaryXPProtoTrait 

-- | Find the trait earning the most XP from the advancement, returning the ProtoTrait
primaryXPProtoTrait :: Advancement -> Maybe ProtoTrait
primaryXPProtoTrait = mhead .  sortOn ((*(-1)) . fromMaybe (-1) . xp) 
                    . filter (isJust . xp) . getAA . changes

-- | Validate allocation of XP.
validateXP :: Augmented Advancement -> Augmented Advancement
validateXP a = addValidation (xpValidation a) a

-- | Validate allocation of XP.
xpValidation :: Augmented Advancement -> [ Validation ]
xpValidation a 
    | isNothing sq' && xpsum > 0 = [ ValidationWarning $ "Undefined Source Quality. Spent " ++ showNum xpsum ++ "xp." ]
    | sq > xpsum = [ ValidationError $ "Underspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "." ]
    | sq < xpsum = [ ValidationError $ "Overspent " ++ showNum xpsum ++ "xp of " ++ showNum sq ++ "." ]
    | otherwise = [ Validated $ "Correctly spent " ++ showNum sq ++ " xp." ]
    where xpsum = spentXP a
          sq = fromMaybe 0 $ effectiveSQ a
          sq' =  effectiveSQ a

