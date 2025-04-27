{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Advancement
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Advancement of Sagas, Covenants, and Characters.
--
--
-----------------------------------------------------------------------------
module ArM.Advancement ( advanceSaga 
                       , Advance(..)
                       , Validation(..)
                       ) where

import Data.Maybe 
import Data.List 

-- import ArM.Cov.Saga
import ArM.Char.Character
-- import ArM.Types.Trait
import ArM.Types.Covenant
-- import ArM.Types.Character
import ArM.Types.Library
import ArM.Types
import ArM.Types.Saga
import ArM.Helper

-- |
-- = Advancement

-- |
-- The Advance class represents objects which change state from
-- season to season.
--
-- Normally only Saga objects should be advanced explicitly.
-- Other instances exist for internal use in the Saga instance.
class Timed a => Advance a where

    -- | Next season - if this is undefined (e.g. at GameStart), the time of the next
    -- advancement is returned.
    nextSeason :: a -> SeasonTime
    nextSeason c | ssn == GameStart = ns
                 | ssn == NoTime = ns
                 | otherwise = seasonNext ssn
       where ssn = season c
             ns = nextAdvancement c
    -- | Next season with an advancement defined
    nextAdvancement :: a -> SeasonTime
    -- | The prepare function is applied when the object is read from file
    prepare :: a -> a
    prepare = id


-- |
-- == Saga Advancement

-- | The saga can be advanced with the same class methods as
-- characters and covenants.  When the saga advances, all its
-- characters and covenants advance accordingly.
instance Advance Saga where
   nextAdvancement saga  = min charnext covnext 
      where charnext = foldl min NoTime [ nextAdvancement x | x <- characters st ]
            covnext = foldl min NoTime [ nextAdvancement x | x <- covenants st ]
            st = sagaState saga

-- | Advance the saga forward by one season.
stepSaga :: Saga -> Saga
stepSaga saga = saga { sagaState = st' }
     where st' = st { stateTitle = stateTitle st 
                    , seasonTime = ns
                    , covenants = cov
                    , characters = ch
                    }
           st = sagaState saga
           (cov,ch) = jointAdvance saga ((covenants st),(characters st))
           ns = nextSeason saga


-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: Saga -> [ Saga ]
advanceSaga saga = reverse $ saga:advanceSaga' ts saga
   where ts = seasons $ sagaFile saga

advanceSaga' :: [SeasonTime] -> Saga -> [ Saga ]
advanceSaga' [] _ = []
advanceSaga' (t:ts) saga0 = n:advanceSaga' ts n
    where n = f t saga0
          f ssn saga | NoTime == nextSeason saga = saga 
                     | ssn < nextSeason saga = saga 
                     | otherwise = f ssn $ stepSaga saga

-- |
-- Advance listed covenants and characters one season forward.
-- The advancement happens jointly, with several passes, to resolve
-- inter-dependencies.
jointAdvance :: Saga         -- ^ Saga reference, passed to know what the next season is
             -> ([Covenant],[Character]) -- ^ Lists of prior covenants and characters
             -> ([Covenant],[Character]) -- ^ Lists of future covenants and characters
jointAdvance saga = completeJoint . addBooks . advJoint . nextJoint saga

-- | Convenience type for joint advancement
type CovAA = (Covenant,Maybe AugCovAdvancement)
-- | Convenience type for joint advancement
type ChaAA = (Character,Maybe AugmentedAdvancement)

-- |
-- Get the next advancements, preparing for joint advancement
nextJoint :: Saga -> ([Covenant],[Character]) -> ([CovAA],[ChaAA]) 
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
-- == Covenant

-- |
-- The `Advance` instance is very similar to that of `Character`, but has to
-- be implemented separately to account for different advancement classes.
instance Advance Covenant where
   nextAdvancement c = f $ futureCovAdvancement c
       where f [] = NoTime
             f (x:_) = caSeason x
   prepare x = f x
        where f y | isNothing (covenantState y) = y { covenantState = Just defaultCovState }
                  | otherwise = y 

-- | Apply covenant advancement
applyCovAdv :: (Covenant,Maybe AugCovAdvancement)
         -> (Covenant,Maybe AugCovAdvancement)
applyCovAdv (c,Nothing) = (c,Nothing)
applyCovAdv (c,Just a) = (c',Just a)
    -- where (a',st') = applyCovAdvancement a st
    where st' = st { covTime = caSeason aa, covenFolkID = cid }
          c' = c { covenantState = Just st' }
          st = fromMaybe defaultCovState $ covenantState c
          cid1 = sort $ joining aa ++ covenFolkID st 
          cid = cid1 -= ( sort $ leaving aa )
          aa = contractAdvancement a

-- |
-- == Character Advancement
-- 
-- Character advancement is divided conceptually into CharGen (pre-game advancement)
-- and in-game advancement.  CharGen is handled by the `prepare` function, which
-- is implemented by `ArM.Char.Character.prepareCharacter`.
--
-- In-game advancement is managed through saga advancement, which uses three different
-- steps from the character advancement definitions:
-- + nextAdv
-- + applyAdv
-- + completeAdv
--
instance Advance Character where

   nextAdvancement = f . futureAdvancement
       where f [] = NoTime
             f (x:_) = season x
   -- | Compute the initial state if no state is recorded.
   -- The function uses `applyCGA` to process all of the pregame advancements.
   -- It then calls `addConfidence` to add the confidence trait to the state
   -- for the returned `Character` object
   prepare = prepareCharacter


-- |
-- Apply the next augmented advancement.
--
-- The main process is defined by the `applyAdvancement` function from
-- `ArM.Char.Advancement`
applyAdv :: (Character,Maybe AugmentedAdvancement)
         -> (Character,Maybe AugmentedAdvancement)
applyAdv (c,Nothing) = (c,Nothing)
applyAdv (c,Just a) = (c',Just a')
    where (a',st') = applyAdvancement a st
          c' = c { state = Just st' }
          st = fromMaybe defaultCS $ state c

-- |
-- Complete the advancement step and tidy up
completeAdv :: (Character,Maybe AugmentedAdvancement)
                 -> Character
completeAdv (c,Nothing) = c 
completeAdv (c,Just a) = c { pastAdvancement = a:pastAdvancement c }

-- |
-- Get the next augmented advancement.
--
-- The main process is defined by the `prepareAdvancement` function from
-- `ArM.Char.Advancement`
nextAdv :: SeasonTime -> Character -> (Character,Maybe AugmentedAdvancement)
nextAdv ns ch | fs == [] = (ch,Nothing)
              | season adv > ns = (ch,Nothing)
              | otherwise = (new,Just a)
        where a = prepareAdvancement (fromJust st) adv
              st = state ch
              (adv:as) = fs
              fs = futureAdvancement ch
              new = ch { futureAdvancement = as }
