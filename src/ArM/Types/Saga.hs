{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Types and basic functions to handle sagas, that is the
-- top level object of the data model
--
-----------------------------------------------------------------------------
module ArM.Types.Saga where

import ArM.Internal.Aux

import           Swish.RDF.Graph as G
import qualified Swish.RDF.Query as Q
-- import qualified Swish.RDF.VarBinding as VB 
import           Swish.VarBinding  (vbMap)
import           Data.Aeson
-- import           Data.Maybe (fromJust)
-- import           Data.List (sort)
-- import qualified ArM.KeyPair as KP
-- import           ArM.Types.Character 
import           ArM.Types.RDF
import           ArM.Rules.Aux
import           ArM.Resources
import           ArM.KeyPair

data Saga = Saga { sagaID :: RDFLabel
                 , sagaTitle :: String
                 , schemaFile :: String
                 , resourceFiles :: [String]
                 , characterFiles :: [String]
                 , sagaGraph :: RDFGraph
                 }

instance ToJSON Saga where 
    toJSON c = toJSON $ p x xs
        where x = KeyValuePair (armRes "sagaID") $ sagaID c
              xs = fromRDFGraph ( sagaGraph c ) ( sagaID c )
              p y (KeyPairList ys) = KeyPairList (y:ys) 

defaultSaga :: Saga 
defaultSaga = Saga { sagaID = armRes "noSuchSaga"
                 , sagaTitle = "No Title"
                 , schemaFile = "/dev/null"
                 , resourceFiles = []
                 , characterFiles = []
                 , sagaGraph = emptyGraph
                 }


-- | Get the labels of all sagas in a given graph.
sagaFromGraph :: G.RDFGraph -> [G.RDFLabel]
sagaFromGraph = uniqueSort . f . map (`vbMap` cVar) . parsegraph
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
          parsegraph = Q.rdfQueryFind 
                     $ listToRDFGraph  [ arc cVar typeRes (armRes "Saga") ]

getFiles :: String -> RDFLabel -> RDFGraph -> [String]
getFiles ft s = f . map rdfToString . f 
                   . map (`vbMap` (Var "file")) . parsegraph 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
          parsegraph = Q.rdfQueryFind $ listToRDFGraph  [ a ]
          a = arc s (armRes ft) (Var "file") 
getResourceFiles :: RDFLabel -> RDFGraph -> [String]
getResourceFiles = getFiles "hasResourceFile"
getSchemaFiles :: RDFLabel -> RDFGraph -> [String]
getSchemaFiles = getFiles "hasSchemaFile"
getCharacterFiles :: RDFLabel -> RDFGraph -> [String]
getCharacterFiles = getFiles "hasCharacterFile"

getSagaTitle :: RDFLabel -> RDFGraph -> String
getSagaTitle s = f1 . map rdfToString . f 
                   . map (`vbMap` (Var "label")) . parsegraph 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
          f1 [] = error "Saga has no title"
          f1 (Nothing:_) = error "Saga title does not parse"
          f1 (Just x:_) = x
          parsegraph = Q.rdfQueryFind $ listToRDFGraph  [ a ]
          a = arc s (armRes "hasLabel") (Var "label") 
