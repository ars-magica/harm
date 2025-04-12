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
printStateHistory st = printAdvHistory t ( chrh, covh )
    where covh = map pastCovAdvancement $ trace "Cov" $ covenants st
          chrh = map pastAdvancement $ trace "Char" $ characters st
          t = season st

printAdvHistory :: SeasonTime
                -> ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) 
                -> [ ( SeasonTime, OList ) ]
printAdvHistory t = printAdvHistory' . sortAdvHistory t

printAdvHistory' :: [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] 
                 -> [ ( SeasonTime, OList ) ]
printAdvHistory' xs = trace ("AH "++ show (length xs)) $ map printOneSeason xs

printOneSeason :: ( [ AugmentedAdvancement ], [ AugCovAdvancement ] )
                 -> (SeasonTime,OList)
printOneSeason (xs,ys) = trace ("One "++show xs) (t,OList $ map printMD ys ++ map printMD xs )
     where t | xs /= [] = season $ head xs
             | ys /= [] = season $ head ys
             | otherwise = NoTime

sortAdvHistory :: SeasonTime
               -> ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) 
               -> [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] 
sortAdvHistory t (xs,ys) = zzip chs cvs
   where chs = stripAdvs t xs
         cvs = stripAdvs t ys
	 zzip [] [] = []
	 zzip (x:xs) [] = (x,[]):zzip xs []
	 zzip [] (x:xs) = ([],x):zzip [] xs 
	 zzip (x:xs) (y:ys) = (x,y):zzip xs ys

stripAdv :: (Show a,Timed a) => SeasonTime -> [ a ] -> (Maybe a,[a])
stripAdv _ [] = (Nothing, [])
stripAdv t (x:xs) | season x < t = trace "strip Nothing" (Nothing, xs)
                  | otherwise = trace (show x) (Just x, xs)

stripAdvs :: (Eq a,Show a,Timed a) => SeasonTime -> [[a]] -> [[a]]
stripAdvs _ [] = []
stripAdvs t as = trace ("strip "++show t) $ cur: stripAdvs (seasonPrev t) ltr
    where cur = filterNothing $ map fst xs
          ltr = filter (/=[]) $ map snd xs
          xs = map (stripAdv t) as


