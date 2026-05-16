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

import ArM.Trait
import ArM.Types.Covenant
import ArM.Story
import ArM.Helper
import Data.List
import Data.Maybe

-- * Processing Books

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

-- * Lab Texts

-- | Filter out grimoires, that is books with multiple lab texts
filterGrimoires :: [ Possession ] -> [ Possession ]
filterGrimoires = sort . nonBook . filter ( (1<) . length . labTexts ) 

nonBook :: [ Possession ] -> [ Possession ]
nonBook = filter ( not . isBook )

-- | Filter out scrolls, that is items with a single lab text
filterScrolls ::  [ Possession ] -> [ (LabText,Possession) ]
filterScrolls = sort . f2 . f1 . nonBook . filter ( (1==) . length . labTexts ) 
    where f2 [] = []
          f2 (([],_):xs) = f2 xs
          f2 ((x:[],ps):xs) = (x,ps):f2 xs
          f2 ((_,_):xs) = f2 xs
          f1 x = zip (map labTexts x) x

-- * The Library Object

-- | A library organises books (represented as 'Possession' objects) into sections.
data Library = Library { libraryName :: String
                       , libraryTime :: SeasonTime
                       , antologies :: [ Possession ]  
                         -- ^ books covering multiple topics
                       , artBooks :: [ Possession ]
                       , abilityBooks :: [ Possession ]
                       , otherBooks :: [ Possession ]  
                       , grimoires :: [ Possession ]  
                       , spellTexts :: [ Possession ]  
                       , itemTexts :: [ Possession ]  
                         -- ^ books which do not fit in the other sections 
                         -- (should be empty)
                       }
-- | Empty library
defaultLibrary :: Library
defaultLibrary = Library "Anonymous Library" NoTime [] [] [] [] [] [] []

-- | Sort a list of possessions into an organised library, ignoring non-book
-- possessions.
groupBooks :: [ Possession ] -> Library
groupBooks = addBooks defaultLibrary 

-- | Sort a list of possessions into an the given 'Library' object,
-- ignoring non-book possessions.
addBooks :: Library -> [ Possession ] -> Library
addBooks lib = addBooks' lib . reverse . sort . filterBooks

-- | Auxiliary for addBooks
addBooks' :: Library -> [ KeyedBook ] -> Library
addBooks' l [] = l
addBooks' l (p:ps) = addBooks' (addBook l p) ps

-- | Auxiliary for addBooks'
addBook :: Library -> KeyedBook -> Library
addBook l (True,_,_,_,p) = l { antologies = p:antologies l }
addBook l (False,ArtKey _,_,_,p) =  l { artBooks = p:artBooks l }
addBook l (False,AbilityKey _,_,_,p) =  l { abilityBooks = p:abilityBooks l }
addBook l (_,_,_,_,p) = l { otherBooks = p:otherBooks l }

-- | get the library from a given 'Covenant'.
getLibrary :: Covenant -> Library
getLibrary cov = addScrolls s $ addGrimoires g $ addBooks lib ps
   where lib = defaultLibrary { libraryName = "Library at " ++ name cov
                              , libraryTime = season cov }
         ps = fromMaybe [] $ fmap possessions  $ covenantState cov
         g = filterGrimoires ps
         s = filterScrolls ps
addGrimoires :: [ Possession ] -> Library -> Library 
addGrimoires [] lib = lib
addGrimoires (p:ps) lib = addGrimoires ps $ lib { grimoires = p:grimoires lib }
addScrolls :: [ (LabText,Possession) ] -> Library -> Library 
addScrolls [] lib = lib
addScrolls ((_,p):ps) lib = addScrolls ps $ lib { spellTexts = p:spellTexts lib }

instance HarmObject Library where
instance Timed Library where
    season = libraryTime
instance StoryObject Library where
    name = libraryName
