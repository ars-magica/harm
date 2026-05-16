-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Sheet.Library
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to sort book listings.
--
-- This module is agnostic of the output format.  It will only group, sort,
-- and categorise books.
--
-----------------------------------------------------------------------------
module ArM.Sheet.Library where

import ArM.Types
import ArM.Types.Trait
import ArM.Types.Covenant
import ArM.Types.HarmObject()
import ArM.Helper
import Data.List
import Data.Maybe

{-
 - Book categories
 - - Art
 - - Ability
 -}

-- | The 'KeyedBook' is a possession representing a book together with
-- various key indicators for sorting and classification.
type KeyedBook = ( Bool, TraitKey, Maybe BookStats, Bool, Possession ) 

-- | Filter out the books from a list of possessions and add a search key.
filterBooks :: [ Possession ] -> [ KeyedBook ]
filterBooks [] = []
filterBooks (x:xs) = f $ posBookTopic x
     where f (b:bs) = (0<length bs,b,bookSortKey x,isComposite x,x):filterBooks xs
           f [] = filterBooks xs

-- | Get the book stats from a possession
posBookStat :: Possession -> [ BookStats ]
posBookStat = foldl (++) [] . map bookStats . bookTexts

-- | Get the book stats from a possession
bookSortKey :: Possession -> Maybe BookStats 
bookSortKey = mhead . foldl (++) [] . map bookStats . bookTexts

-- | Get the book topics from a possession
posBookTopic :: Possession -> [ TraitKey ]
posBookTopic = uniqueSort . map topic . posBookStat

-- | A library organises books (represented as 'Possession' objects) into sections.
data Library = Library { libraryName :: String
                       , libraryTime :: SeasonTime
                       , antologies :: [ Possession ]  -- ^ books covering multiple topics
                       , artBooks :: [ Possession ]
                       , abilityBooks :: [ Possession ]
                       , otherBooks :: [ Possession ]  
                         -- ^ books which do not fit in the other sections (should be emoty)
                       }
-- | Empty library
defaultLibrary :: Library
defaultLibrary = Library "Anonymous Library" NoTime [] [] [] []

-- | Sort a list of possessions into an organised library, ignoring non-book
-- possessions.
groupBooks :: [ Possession ] -> Library
groupBooks = addBooks defaultLibrary 

-- | Sort a list of possessions into an the given 'Library' object,
-- ignoring non-book possessions.
addBooks :: Library -> [ Possession ] -> Library
addBooks lib = addBooks' lib . sort . filterBooks

-- | Auxiliary for addBooks
addBooks' :: Library -> [ KeyedBook ] -> Library
addBooks' l [] = l
addBooks' l (p:ps) = addBooks' (addBook l p) ps

-- | Auxiliary for addBooks'
addBook :: Library -> KeyedBook -> Library
addBook l (True,_,_,_,p) = l { antologies = p:antologies l }
addBook l (False,ArtKey _,_,_,p) = l { artBooks = p:artBooks l }
addBook l (False,AbilityKey _,_,_,p) = l { abilityBooks = p:abilityBooks l }
addBook l (_,_,_,_,p) = l { otherBooks = p:otherBooks l }

-- | get the library from a given 'Covenant'.
getLibrary :: Covenant -> Library
getLibrary cov = addBooks lib ps
   where lib = defaultLibrary { libraryName = "Library at " ++ name cov
                              , libraryTime = season cov }
         ps = fromMaybe [] $ fmap possessions  $ covenantState cov

instance HarmObject Library where
instance Timed Library where
    season = libraryTime
instance StoryObject Library where
    name = libraryName
