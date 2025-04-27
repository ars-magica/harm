{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Cov.Saga
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Saga type with references to constituent files and objects.
--
--
-----------------------------------------------------------------------------
module ArM.Cov.Saga ( Saga(..)
                    , SagaFile(..)
                    , SagaState(..)
                    , advanceSaga
                    , characterIndex
                    , covenantIndex
                    , advancementErrors
                    , advancementErrorsLimit
                    , covenFolk
                    , sagaState
                    ) where

import Data.Maybe 
import Data.List 

import ArM.Char.Character
import ArM.Types.Covenant
import ArM.Types.Library
import ArM.Types.HarmObject
import ArM.Types.Saga
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- == Error reports

-- | Get an `OList` of all error messages from past advancements in a
-- saga state.
--
-- CharGen errors are only included at GameStart and ignored later.
advancementErrors :: SagaState -> OList
advancementErrors saga | errors == [] = OString "No errors"
                       | otherwise = OList $ map formatOutput errors
    where formatOutput (cid,_,ssn,vs) = OList 
              [ OString ( cid ++ ": " ++ ssn ),
              OList $ map msg vs ]
          errors = errorList saga
          msg (ValidationError x) = OString x
          msg _ = OString ""

-- | Convenience type for a list of validation messages for a 
-- given cvharacter and season
type VList = (String,SeasonTime,String,[Validation])

-- | Did the `VList` object occur after the given season?
errorAfter :: VList -> SeasonTime -> Bool
errorAfter (_,vs,_,_) s = vs > s 

-- | Exctract all validation errors from previous advancements at
-- a given saga state
errorList :: SagaState -> [VList]
errorList saga = sortOn ( \ (_,x,_,_) -> x ) vvs
    where cs = characters saga
          cvs = map cErrors  cs
          vvs = g cvs 
          g = f . filterVList . foldl (++) [] 
          f [] = []
          f ((_,_,_,[]):xs) = f xs
          f (x:xs) = x:f xs

-- | Extract only errors from a list of `VList` objects.
filterVList :: [VList] -> [VList]
filterVList = map ( \ (a,b,c,d) -> (a,b,c,filterError d) ) 

-- | Get errors from a list of Validation objects
filterError :: [Validation] -> [Validation]
filterError (Validated _:xs) = filterError xs
filterError (x:xs) = x:filterError xs
filterError [] = []

-- | Exctract a list of validation errors after a given time 
-- This is not currently used, but could be used to ignore old
-- errors when reporting recent character states.
advancementErrorsLimit :: SeasonTime ->  SagaState -> OList
advancementErrorsLimit ssn saga = OList $ map formatOutput errors
    where formatOutput (cid,_,sn,vs) = OList 
              [ OString ( cid ++ ": " ++ sn ),
              OList $ map msg vs ]
          errors = f $ errorList saga
          msg (ValidationError x) = OString x
          msg _ = OString ""
          f [] = []
          f (x:xs) | x `errorAfter` ssn = x:f xs
                   | otherwise = []

-- | Get validation messages from a given advancement.
-- Auxiliary for `cErrors`
aaErrors :: Character -> AugmentedAdvancement -> VList
aaErrors c a = (charID c, season a, augHead a, vs )
    where vs = validation  a

-- | Get validation messages from a given character.
-- Auxiliary for `listErrors`
cErrors :: Character -> [VList]
cErrors c = map (aaErrors c) as
   where as | ps == [] = pregameDesign c
            | otherwise = ps
         ps = pastAdvancement c

-- | Format a header for `renderCharErrors`
augHead :: AugmentedAdvancement -> String
augHead a = augHead' (season a) (mode a)
-- | Format a header for `renderCharErrors`
augHead' :: SeasonTime -> AdvancementType -> String
augHead' NoTime tp = show tp
augHead' x tp = (show x  ++ " " ++ show tp)


-- | Character Index
-- ==

-- | Write a single item for `characterIndex`
characterIndexLine :: Character -> OList
characterIndexLine c = OString $ "+ " ++ pagesLink (stateName c) 

-- | Write a bullet list of links for a list of characters
characterIndex :: [Character] -> OList
characterIndex = OList . map characterIndexLine 

-- | Write a single item for `covenantIndex`
covenantIndexLine :: Covenant -> OList
covenantIndexLine c = OString $ "+ " ++ pagesLink (stateName c) 

-- | Write a bullet list of links for a list of characters
covenantIndex :: [Covenant] -> OList
covenantIndex = OList . map covenantIndexLine 

-- |
-- = Advancement

-- | The saga can be advanced with the same class methods as
-- characters and covenants.  When the saga advances, all its
-- characters and covenants advance accordingly.
instance Advance Saga where
   -- advance :: SeasonTime -> a -> a
   advance t saga = saga { sagaState = advance t (sagaState saga) }

   -- step :: a -> a
   step saga = saga { sagaState = step (sagaState saga) }

   -- nextSeason :: a -> SeasonTime
   nextSeason = nextSeason . sagaState

-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: Saga -> [ Saga ]
advanceSaga saga = reverse $ saga:advanceSaga' ts saga
   where ts = seasons $ sagaFile saga

advanceSaga' :: [SeasonTime] -> Saga -> [ Saga ]
advanceSaga' [] _ = []
advanceSaga' (t:ts) x = trace ("adv> " ++ show t) $ n:advanceSaga' ts n
    where n = advance t x

instance Advance SagaState where
   advance t saga 
      | NoTime == ns = saga 
      | t < ns = saga 
      | otherwise = advance t $ step saga
     where ns = nextSeason saga

   step saga = saga { stateTitle = stateTitle saga 
                    , seasonTime = ns
                    , covenants = cov
                    , characters = ch
                    }
     where (cov,ch) = jointAdvance saga ((covenants saga),(characters saga))
           ns = nextSeason saga

   nextSeason saga = foldl min NoTime ss
      where ss = [ nextSeason x | x <- characters saga ]

-- |
-- Advance listed covenants and characters one season forward.
-- The advancement happens jointly, with several passes, to resolve
-- inter-dependencies.
jointAdvance :: SagaState         -- ^ Saga reference, passed to know what the next season is
             -> ([Covenant],[Character]) -- ^ Lists of prior covenants and characters
             -> ([Covenant],[Character]) -- ^ Lists of future covenants and characters
jointAdvance saga = completeJoint . addBooks . advJoint . nextJoint saga

-- | Convenience type for joint advancement
type CovAA = (Covenant,Maybe AugCovAdvancement)
-- | Convenience type for joint advancement
type ChaAA = (Character,Maybe AugmentedAdvancement)

-- |
-- Get the next advancements, preparing for joint advancement
nextJoint :: SagaState -> ([Covenant],[Character]) -> ([CovAA],[ChaAA]) 
nextJoint saga (xs,ys) = (map (nextCovAdv ns) xs,map (nextAdv ns) ys)
           where ns = nextSeason saga

-- |
-- Complete the joint advancement of characters and covenants.
completeJoint :: ([CovAA],[ChaAA]) -> ([Covenant],[Character])
completeJoint (xs,ys) = (map completeCovAdv xs,map completeAdv ys)

-- |
-- Jointly advance characters and covenants.
advJoint :: ([CovAA],[ChaAA]) -> ([CovAA],[ChaAA]) 
advJoint (xs,ys) = (map applyCovAdv xs, map applyAdv ys)

-- |
-- Find books in the covenants and add to the advancements for characters
-- who use them.
addBooks :: ([CovAA],[ChaAA]) -> ([CovAA],[ChaAA]) 
addBooks (xs,ys) = validateBooks (xs,map (addBook xs') ys)
   where xs' = map fst xs

-- |
-- Validate the use of books.
validateBooks :: ([CovAA],[ChaAA]) -> ([CovAA],[ChaAA]) 
validateBooks (xs,ys) = (xs, f ys)
   where vs = map valGBU $ getBookUse ys
         f [] = []
         f ((ch,aa):s) 
            | isNothing aa = (ch,aa):s
            | bookUsed (fromJust aa) == [] = (ch,aa):s
            | otherwise = (ch,Just $ g (fromJust aa) vs):s
         g aa [] = aa
         g aa ((x,v):s) 
           | x `elem` bookUsed aa = g (aa { validation = v:validation aa }) s
           | otherwise = g aa s

-- | Validate use of a single book.
-- This is an auxiliary for `validateBooks` which applies it with `map`.
valGBU :: (Book,[Character]) -> (Book,Validation)
valGBU (b,cs) | bookCount b < length cs = (b, ValidationError err )
              | otherwise = (b,Validated $ "Book " ++ bookID b ++ " is available.")
    where err = "Oversubscription " ++ show (bookCount b) ++ " copies of " ++ (bookID b)
                    ++ ". Used by " ++ showStrList (map name cs) ++ "." 

-- |
-- Get a list of book uses for validation.
getBookUse :: [ChaAA] -> [ ( Book, [Character] ) ]
getBookUse = f5 . f4 . f3 . f2 . f1
   where f1 = map  ( \ (ch,aa) -> ( bu aa, ch ) )
         f2 = map ( \ (bs, ch) -> [ (b,ch) | b <- bs ] ) 
         f3 = sortOn fst . foldl (++) [] 
         bu = fromMaybe [] . fmap bookUsed
         f4 = map ( \ (x,y) -> ( x, [y] ) )
         f5 [] = []
         f5 (x:[]) = x:[]
         f5 ((x1,y1):(x2,y2):s) 
             | x1 == x2 = f5 ((x1,y1++y2):s)
             | otherwise = (x1,y1):f5 ((x2,y2):s)

    
-- |
-- Find books in the covenants and add to the advancement of the given
-- character if they use the book.
addBook :: [Covenant] -> ChaAA -> ChaAA
addBook cvs (x,y) = (x,fmap (addBook' cov) y)
   where cov =  findCov x cvs

-- |
-- Find and add books with stats to add to the character advancement.
-- Not implemented yet.
addBook' :: Maybe Covenant -> AugmentedAdvancement -> AugmentedAdvancement
addBook' Nothing y  = y
addBook' (Just cov) y = f bs y
    where u = usesBook y
          bk | isNothing st = [ Nothing | _ <- u ]
             | otherwise = map (findBook (fromJust st)) u
          bs = zip u bk
          st = covenantState cov
          f [] aa = aa
          f ((bid,Nothing):xs) aa = f xs $ aa { validation = nobk bid:validation aa }
          f ((_,Just b):xs) aa = f xs $ aa { bookUsed = b:bookUsed aa }
          nobk x = ValidationError $ "Book not found (" ++ x ++ ")"

-- |
-- == Covenant support

-- |
-- List of covenFolk as `Character` objects at the covenant
covenFolk :: Saga -> CovenantState -> [ Character ]
covenFolk saga cov = lookupCharacters s $ f cov
   where f = covenFolkID 
         s = saga

-- |
-- Find `Character` objects for a list of character IDs, from the given `Saga`.
lookupCharacters :: Saga -> [ HarmKey ] -> [ Character ]
lookupCharacters saga is = harmLookup is cs
    where cs = sortOnKey $ characters $ sagaState saga

