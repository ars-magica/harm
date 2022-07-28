{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.Metadata
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Handling character metadata
--
-----------------------------------------------------------------------------

module ArM.Character.Metadata ( getCharacterMetadata, characterFromGraph ) where
 

import ArM.Rules.Aux
import ArM.Resources
import ArM.KeyPair

import Swish.RDF.Graph as G
import Swish.RDF.Query as Q
import Swish.RDF.VarBinding as VB 
import Swish.VarBinding 
import qualified Data.Set as DS
import Data.List (sort)




-- | Construct the query for a given character 'c', for use
-- with the following functions .
query c = qparse $  prefixes
     ++ " " ++ c ++ " ?property ?value . "
     ++ " ?property rdf:type  arm:CharacterProperty . "
     ++ " ?property rdfs:label ?label . "


-- | Find all characters in a given graph.  Auxiliary for `characterFromGraph`.
characterFromGraph' :: RDFGraph -> [VB.RDFVarBinding]
characterFromGraph' = Q.rdfQueryFind
             $ toRDFGraph $ DS.fromList [ arc cVar typeRes armCharacter ]
-- | Get the labels of all characters in a given graph.
characterFromGraph :: RDFGraph -> [RDFLabel]
characterFromGraph = uniqueSort . f . map (`vbMap` cVar) . characterFromGraph' 
    where f [] = []
          f (Nothing:xs) = f xs
          f (Just x:xs) = x:f xs
-- | Sort the list and remove duplicates.
uniqueSort = f . sort
    where f [] = []
          f (x:[]) = x:[]
          f (x:y:ys) | x == y = f (y:ys)
          f (x:y:ys) | x /= y = x:f (y:ys)
-- [ arc c (G.Var "property") (G.Var "value"),
-- , arc (G.Var "property") typeRes armCharacterProperty,
-- , arc (G.Var "property") labelRes  (G.Var "label") ]

-- | Make a list of metadata, where each data item is
-- a triple consisting of URI, Label, and Value.
-- The inputs are an 'RDFGraph' g and a string naming an RDF resource,
-- either as a prefixed name or as a full URI in angled brackets (<uri>).
getCharacterMetadata :: G.RDFGraph -> String -> KeyPairList
getCharacterMetadata g s = KeyPairList $ map keypairFromBinding
                          $  getCharacterMetadataVB g s

-- | Get the variable bindings from the graph.
getCharacterMetadataVB :: G.RDFGraph -> String -> [VB.RDFVarBinding]
getCharacterMetadataVB g c = Q.rdfQueryFind (query c) g

