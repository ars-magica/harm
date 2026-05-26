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
module ArM.Markdown.SeasonDisplay where

import ArM.Types.Advancement
import ArM.Types.Harm 
import ArM.Story
import ArM.Saga
import ArM.Markdown.HOutput
import ArM.Markdown.HList
import ArM.Helper
import Data.HList

-- import ArM.Debug.Trace

-- | Return the the annals of the saga, represented as a list of 
-- `AnnalSeason` objects, each of which comprising the events of one season.
sagaAnnals :: Saga -> [ AnnalSeason ]
sagaAnnals = getSeasonAnnals . getAugMerged

-- | The `AnnalSeason` object collects all events of a season,
-- represented as a list of advancement objects for characters and
-- covenants.
data AnnalSeason = AnnalSeason SeasonTime [EitherAug]

instance Timed AnnalSeason where
   season (AnnalSeason t _) = t

instance HOutput AnnalSeason where
   printH (AnnalSeason t xs) = Just $ indentList $ HList h 
                             $ filterNothing $ map printH xs
      where h = "## " ++ show t

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


data CharAug = CharAug Character (Augmented Advancement)
   deriving ( Show, Eq )
instance Timed CharAug where
   season (CharAug _ a) = season a
data CovAug = CovAug Covenant (Augmented CovAdvancement)
   deriving ( Show, Eq )
instance Timed CovAug where
   season (CovAug _ a) = season a
data EitherAug = EChar CharAug | ECov CovAug
instance Timed EitherAug where
   season (EChar x) = season x
   season (ECov x) = season x


instance StoryObject CharAug where
   name (CharAug c a) = (name c ++ " (" ++ show (advMode a) ++ ")")
   narrative (CharAug _ a) = narrative a
   comment (CharAug _ a) = comment a
   addNarrative s (CharAug c x) = CharAug c $ addNarrative s x 
   addComment s (CharAug c x) = CharAug c $ addComment s x 

{-
instance StoryObject CovAug where
   name (CovAug c a) = (name c ++ " (" ++ show (mode a) ++ ")")
   narrative (CovAug _ a) = narrative a
   comment (CovAug _ a) = comment a
-}

storyHList :: StoryObject a => a -> HList 
storyHList ob = HList ( name ob ) $ filterNothing [ narrativeH ob, commentH ob ]

instance HOutput CharAug where
   printH (CharAug c a') = Just 
       $ appendToHList ( bk:(filterNothing $ map printH $ validation a' ) )
       $ storyHList (CharAug c a') 
       where a = contractAdvancement a'
             bk = (hlist . ("Uses "++) . show ) $ bookRead  a
instance HOutput CovAug where
   printH (CovAug c a') = f $ filterNothing [ printH a ]
     where a = contractAdvancement a'
           f [] = Nothing
           f xs = Just $ HList (name c) xs
instance HOutput EitherAug where
   printH (ECov x) = printH x
   printH (EChar x) = printH x

-- ** Getting the merged list of advancements

-- | Get a list of all past advancements in a SeasonState, sorted by time.
-- If the merging, defined in `ArM.Char.Types.Advancement` using the lists
-- library is stable, covenants should come first and the ordering of characters
-- and covenants be the same in every season.
getAugMerged :: Saga -> [ EitherAug ]
getAugMerged st = mergeByTime xs'  ys' 
    where (xs,ys) = getAugMerged' st
          xs' = map EChar xs
          ys' = map ECov ys

getAugMerged' :: Saga -> ( [ CharAug ], [ CovAug ] )
getAugMerged' st = ( mergeTimed xs, mergeTimed ys )
    where (xs,ys) = getAug st

getAug :: Saga -> ( [ [ CharAug ] ], [ [ CovAug ] ] )
getAug st = ( chrh, covh )
    where covh = map covAdv $ covenantList st
          chrh = map chAdv  $ characterList st

covAdv :: Covenant -> [ CovAug ]
covAdv c =  map (CovAug c) $ pastCovAdvancement c
chAdv :: Character -> [ CharAug ]
chAdv c =  map (CharAug c) $ pastAdvancement c
 
