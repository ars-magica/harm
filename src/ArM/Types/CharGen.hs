{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.CharGen
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types to handle characters as stored in web server memory.
--
-----------------------------------------------------------------------------
module ArM.Types.CharGen where

import Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
import Data.Maybe
import ArM.KeyPair
import ArM.Resources
import ArM.BlankNode
import ArM.Rules.Aux
import ArM.Types.Character

data CharStage = CharStage {
         stage :: String,
         advancement :: Maybe Advancement,
         sheetObject :: CharacterSheet,
         sheetGraph :: RDFGraph
       }  deriving (Eq)
data CharGen = CharGen {
         charID :: RDFLabel,
         charName :: String,
         charGraph :: RDFGraph,
         charSheets :: [CharStage]
       }  deriving (Eq)
instance Show CharStage where
    show cs = stage cs ++ show (advancement cs)
instance Show CharGen where
    show cs = charName cs ++ " (" ++ (show $ charID cs) ++ ")"
