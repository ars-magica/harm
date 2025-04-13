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
module ArM.SeasonDisplay ( sagaAnnals
                         , AnnalSeason(..)
                         ) where

-- import Data.Maybe 

import ArM.Char.Character 
import ArM.Cov.Saga
import ArM.Types.Covenant
import ArM.Markdown
import ArM.BasicIO
-- import ArM.Helper

-- import ArM.Debug.Trace

sagaAnnals :: SagaState -> [ AnnalSeason ]
sagaAnnals = getSeasonAnnals . getAugMerged

data AnnalSeason = AnnalSeason SeasonTime [EitherAug]

instance Timed AnnalSeason where
   season (AnnalSeason t _) = t

instance Markdown AnnalSeason where
   printMD (AnnalSeason t xs) = OList [ b, h, b, (OList $ map printMD xs) ]
      where b = OString ""
            h = OString $ "## " ++ show t

getSeasonAnnals :: [ EitherAug ] -> [ AnnalSeason ]
getSeasonAnnals [] = []
getSeasonAnnals (x:xs) = (AnnalSeason t zs):getSeasonAnnals ys
    where t = season x
          (ys,zs) = getSeasonAnnals' t ((x:xs),[])


getSeasonAnnals' :: SeasonTime -> ([EitherAug],[EitherAug]) -> ([EitherAug],[EitherAug])
getSeasonAnnals' _ ([],x) = ([],x)
getSeasonAnnals' t (x:xs,ys) 
    | season x /= t = (x:xs,ys)
    | otherwise =  getSeasonAnnals' t (xs,x:ys)


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
-- == Getting the merged list of advancements

-- | Get a list of all past advancements in a SeasonState, sorted by time.
-- If the merging, defined in `ArM.Char.Types.Advancement` using the lists
-- library is stable, covenants should come first and the ordering of characters
-- and covenants be the same in every season.
getAugMerged :: SagaState -> [ EitherAug ]
getAugMerged st = mergeByTime xs'  ys' 
    where (xs,ys) = getAugMerged' st
          xs' = map EChar xs
          ys' = map ECov ys

getAugMerged' :: SagaState -> ( [ CharAug ], [ CovAug ] )
getAugMerged' st = ( mergeTimed xs, mergeTimed ys )
    where (xs,ys) = getAug st

getAug :: SagaState -> ( [ [ CharAug ] ], [ [ CovAug ] ] )
getAug st = ( chrh, covh )
    where covh = map covAdv $ covenants st
          chrh = map chAdv  $ characters st

covAdv :: Covenant -> [ CovAug ]
covAdv c =  map (CovAug c) $ pastCovAdvancement c
chAdv :: Character -> [ CharAug ]
chAdv c =  map (CharAug c) $ pastAdvancement c
 
