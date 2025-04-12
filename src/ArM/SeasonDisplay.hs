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
printStateHistory st = printAdvHistory ( chrh, covh )
    where covh = map pastCovAdvancement $ covenants st
          chrh = map pastAdvancement $ characters st

printAdvHistory :: ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) -> [ ( SeasonTime, OList ) ]
printAdvHistory = printAdvHistory' . sortAdvHistory

sortAdvHistory :: ( [[ AugmentedAdvancement ]], [[ AugCovAdvancement ]] ) 
               -> [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] 
sortAdvHistory _ = []

stripAdv :: Timed a => SeasonTime -> [ a ] -> (Maybe a,[a])
stripAdv _ [] = (Nothing, [])
stripAdv t (x:xs) | season x < t = (Nothing, xs)
                  | otherwise = (Just x, xs)

stripAdvs' :: Timed a => SeasonTime -> [[ a ]] -> [(Maybe a,[a])]
stripAdvs' t = map (stripAdv t)

stripAdvs :: Timed a => SeasonTime -> [(Maybe a,[a])] -> [[a]]
stripAdvs t xs = cur: ( stripAdvs t . stripAdvs' t ) ltr
    where cur = filterNothing $ map fst xs
          ltr = map snd xs


printAdvHistory' :: [ ( [ AugmentedAdvancement ], [ AugCovAdvancement ] ) ] -> [ ( SeasonTime, OList ) ]
printAdvHistory' _ = []
