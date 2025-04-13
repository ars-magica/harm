{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.SeasonDisplay
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Joint season logs for all characters and covenants.
--
-----------------------------------------------------------------------------
module ArM.SeasonDisplay where

-- import Data.Maybe 

import ArM.Char.Character 
import ArM.Cov.Saga
import ArM.Types.Covenant
import ArM.Markdown
-- import ArM.GameRules
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

printAnnals :: Saga -> [ ( SeasonTime, OList ) ]
printAnnals = trace "printAnnals" . printStateHistory . sagaState

printStateHistory :: SagaState -> [ ( SeasonTime, OList ) ]
printStateHistory = printAugMerged . getAugMerged

printAugMerged :: [ EitherAug ] -> [ ( SeasonTime, OList ) ]
printAugMerged [] = []
printAugMerged (x:xs) = (t, OList zs):printAugMerged ys
    where t = season x
          (ys,zs) = printAugMerged' t ((x:xs),[])


printAugMerged' :: SeasonTime -> ([EitherAug],[OList]) -> ([EitherAug],[OList])
printAugMerged' _ ([],x) = ([],x)
printAugMerged' t (x:xs,ys) 
    | season x /= t = (x:xs,ys)
    | otherwise =  printAugMerged' t (xs,printMD x:ys)


-- |
-- == Types

data CharAug = CharAug Character AugmentedAdvancement
   deriving ( Show, Eq )
instance Timed CharAug where
   season (CharAug _ a) = season a
data CovAug = CovAug Covenant AugCovAdvancement
   deriving ( Show, Eq )
instance Timed CovAug where
   season (CovAug _ a) = season a
data EitherAug = EChar CharAug | ECov CovAug
instance Timed EitherAug where
   season (EChar x) = season x
   season (ECov x) = season x

instance Markdown CharAug where
   printMD (CharAug c a) = OList [ OString ("+ " ++ name c)
                                 , printMD a ]
instance Markdown CovAug where
   printMD (CovAug c a) = OList [ OString ("+ " ++ name c)
                                 , printMD a ]
instance Markdown EitherAug where
   printMD (ECov x) = printMD x
   printMD (EChar x) = printMD x

-- |
-- == Getting the merges list of advancements

-- | Get a list of all past advancements in a SeasonState, sorted by time.
-- If the merging, defined in `ArM.Char.Types.Advancement` using the lists
-- library is stable, covenants should come first and the ordering of characters
-- and covenants be the same in every season.
getAugMerged :: SagaState -> [ EitherAug ]
getAugMerged st = mergeByTime ys' xs' 
    where (xs,ys) = getAugMerged' st
          xs' = map EChar xs
          ys' = map ECov ys

getAugMerged' :: SagaState -> ( [ CharAug ], [ CovAug ] )
getAugMerged' st = ( mergeTimed xs, mergeTimed ys )
    where (xs,ys) = getAug st

getAug :: SagaState -> ( [ [ CharAug ] ], [ [ CovAug ] ] )
getAug st = ( chrh, covh )
    where covh = map covAdv $ trace "Cov" $ covenants st
          chrh = map chAdv $ trace "Char" $ characters st

covAdv :: Covenant -> [ CovAug ]
covAdv c = trace ("covAdv: "++name c) $ map (CovAug c) $ pastCovAdvancement c
chAdv :: Character -> [ CharAug ]
chAdv c = trace ("covAdv: "++name c) $ map (CharAug c) $ pastAdvancement c

