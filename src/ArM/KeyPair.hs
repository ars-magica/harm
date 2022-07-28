{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.KeyPair
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Data Types to handle generic queries
--
-----------------------------------------------------------------------------
module ArM.KeyPair where

import Swish.RDF.Parser.N3 (parseN3fromText)
import Swish.RDF.Graph (RDFLabel)
import qualified Swish.RDF.VarBinding as VB 
import Network.URI (URI)
import Swish.VarBinding  (vbMap)
import Data.Maybe  (fromJust)
import Data.List (sort)
import ArM.Resources

import Swish.RDF.Graph as G
import qualified Data.Text.Lazy as T

-- | Create a query graph from an N3 string.
qparse :: String -> RDFGraph
qparse = either error id . parseN3fromText . T.pack

-- | `KeyValuePair` represents a key/value pair in JSON jargon
-- or a property/object pair in RDF.  It is designed to hold
-- arbitrary data read from the source format.
data KeyValuePair = KeyValuePair RDFLabel RDFLabel
     deriving (Show,Eq,Ord)

-- | A list of KeyPair objects, typically used for property/object pairs.
-- This is made an algebraic data type to allow class instantiation
-- for JSON and Show compatibility, but it is internally stored as a list.
data KeyPairList  = KeyPairList [KeyValuePair]
    deriving Eq
instance Show KeyPairList where
        show (KeyPairList []) = ""
        show (KeyPairList (x:xs)) = "  " ++ show x ++ "\n" 
fromKeyPairList (KeyPairList xs) = xs

-- | `ObjectKeyValue` represents an RDF triple.  It is used
-- as an intermediate container before converting into a format
-- using `KeyValuePair`.  
-- It may be redundant, as it is functionally equivalent to `RDFTriple`.
-- It is retained in case we want to add additional redundant information
-- such as the contents of rdfs:label-s in the future.
data ObjectKeyValue = ObjectKeyValue RDFLabel RDFLabel RDFLabel
     deriving (Show,Eq,Ord)

okvSubj (ObjectKeyValue a _ _) = a
okvPred (ObjectKeyValue _ a _) = a
okvObj (ObjectKeyValue _ _ a) = a

sameKey :: ObjectKeyValue -> ObjectKeyValue -> Bool
sameKey (ObjectKeyValue a _ _) (ObjectKeyValue b _ _) = a == b

data KeyValuePairString = KeyValuePairString RDFLabel String
data ObjectKeyValueString = ObjectKeyValueString RDFLabel RDFLabel String

toKeyPair :: ObjectKeyValue -> KeyValuePair 
toKeyPair (ObjectKeyValue a b c) = (KeyValuePair b c)
toKeyPairList :: [ObjectKeyValue] -> [KeyValuePair]
toKeyPairList = map toKeyPair


-- | Split a list of ObjectKeyValue-s so that pairs belonging
-- to the same resource, -- as defined by the first element,
-- are place in the same constituent list.
keypairSplit :: [ObjectKeyValue] -> [[ObjectKeyValue]]
keypairSplit xs = fst $ keypairSplit' ([],sort xs)

-- | keypairSplit' is a mere auxiliary for 'keypairSplit'
keypairSplit' :: ([[ObjectKeyValue]], [ObjectKeyValue]) 
           -> ([[ObjectKeyValue]], [ObjectKeyValue]) 
keypairSplit' (xs,[]) = (xs,[])
keypairSplit' ([],y:ys) = keypairSplit' ([[y]],ys)
keypairSplit' ((x:xs):xss,y:ys) 
    | sameKey  x y = keypairSplit' ((y:x:xs):xss, ys)
    | otherwise    = keypairSplit' ([y]:(x:xs):xss, ys)

-- | Map variable bindings to triples of (property,label,value)
-- Three variables should be bound, property, label, and value.
keypairFromBinding :: VB.RDFVarBinding -> KeyValuePair
keypairFromBinding = f . metadataFromBinding 
     where 
       f (p,label,value) = KeyValuePair (fromJust p) (fromJust value) 

-- | Step 1. Map the variable bindings to Maybe RDFLabel
metadataFromBinding :: VB.RDFVarBinding 
                 -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
metadataFromBinding vb = (vbMap vb (G.Var "property"),
                          vbMap vb (G.Var "label"),
                          vbMap vb (G.Var "value"))

-- | Map variable bindings to quads of (id,property,label,value)
-- Three variables should be bound, id, property, label, and value.
objectFromBinding :: VB.RDFVarBinding -> ObjectKeyValue
objectFromBinding = f . quadVB 
     where f (id,p,_,value) = ObjectKeyValue 
              (fromJust id) (fromJust p) (fromJust value) 

quadVB :: VB.RDFVarBinding 
        -> (Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel, Maybe RDFLabel)
quadVB vb = (vbMap vb (G.Var "id"),
             vbMap vb (G.Var "property"),
             vbMap vb (G.Var "label"),
             vbMap vb (G.Var "value"))

getStringProperty :: RDFLabel -> [KeyValuePair] -> String
getStringProperty _ [] = ""
getStringProperty k' (KeyValuePair k v:xs) 
   | k' == k  = f (fromRDFLabel v)
   | otherwise      = getStringProperty k' xs
   where f Nothing = ""
         f (Just v) = v

getProperty :: RDFLabel -> [KeyValuePair] -> Maybe RDFLabel
getProperty _ [] = Nothing
getProperty k' (KeyValuePair k v:xs) 
   | k' == k  = Just v
   | otherwise      = getProperty k' xs
