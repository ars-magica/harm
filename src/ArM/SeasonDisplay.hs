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

-- import ArM.Debug.Trace

printAnnals :: Saga -> [ ( SeasonTime, OList ) ]
printAnnals = printStateHistory . sagaState

printStateHistory :: SagaState -> [ ( SeasonTime, OList ) ]
printStateHistory st = printAdvHistory t ( chrh, covh )
    where covh = map pastCovAdvancement $ covenants st
          chrh = map pastAdvancement $ characters st
          t = season st

printAdvHistory :: SeasonTime
                -> ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) 
                -> [ ( SeasonTime, OList ) ]
printAdvHistory t = printAdvHistory' . sortAdvHistory t

printAdvHistory' :: [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] 
                 -> [ ( SeasonTime, OList ) ]
printAdvHistory' = map printOneSeason

printOneSeason :: ( [ AugmentedAdvancement ], [ AugCovAdvancement ] )
                 -> (SeasonTime,OList)
printOneSeason (xs,ys) = (t,OList $ map printMD ys ++ map printMD xs )
     where t | xs /= [] = season $ head xs
             | ys /= [] = season $ head ys
             | otherwise = NoTime

sortAdvHistory :: SeasonTime
               -> ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) 
               -> [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] 
sortAdvHistory t (xs,ys) = zip chs cvs
   where chs = stripAdvs t xs
         cvs = stripAdvs t ys

stripAdv :: Timed a => SeasonTime -> [ a ] -> (Maybe a,[a])
stripAdv _ [] = (Nothing, [])
stripAdv t (x:xs) | season x < t = (Nothing, xs)
                  | otherwise = (Just x, xs)

stripAdvs :: Timed a => SeasonTime -> [[a]] -> [[a]]
stripAdvs t as = cur: stripAdvs (seasonNext t) ltr
    where cur = filterNothing $ map fst xs
          ltr = map snd xs
          xs = map (stripAdv t) as


