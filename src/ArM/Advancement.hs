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
                       , StepAdvance(..)
                       , Validation(..)
                       ) where

import Data.Maybe 
import Data.List 

import ArM.Char.Character
import ArM.Types.Covenant
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
    -- | Compute the initial state if no state is recorded.
    -- The prepare function is applied when the object is read from file.
    -- It is handled differently from in-game advancement, because character
    -- generation is independent of any other characters in the game.
    -- Thus CharGen advancement never has to infer stats from other objects
    -- or check for cross-consistency.
    --
    -- The default implementation is the identity function, which is
    -- sufficient for types which starts with a default state and do
    -- not suport pre-game advancement.
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



-- | Apply the next advancement step.
--
-- The main process is defined by the `applyAdvancement` function from
-- `ArM.Char.Advancement`
applyAdv :: AdvancementStep -> AdvancementStep
applyAdv (CharStep c Nothing) = (CharStep c Nothing) 
applyAdv (CharStep c (Just aa)) = (CharStep c' (Just a')) 
       where (a',st') = applyAdvancement aa st
             c' = c { state = Just st' }
             st = fromMaybe defaultCS $ state c
applyAdv (CovStep c Nothing) = (CovStep c Nothing) 
applyAdv (CovStep c (Just aa')) = (CovStep c' (Just aa')) 
    where st' = st { covTime = caSeason aa, covenFolkID = cid }
          c' = c { covenantState = Just st' }
          st = fromMaybe defaultCovState $ covenantState c
          cid1 = sort $ joining aa ++ covenFolkID st 
          cid = cid1 -= ( sort $ leaving aa )
          aa = contractAdvancement aa'

-- |
-- Get the next advancements, preparing for joint advancement
nextJoint :: Saga -> ([Covenant],[Character]) -> ([AdvancementStep],[AdvancementStep]) 
nextJoint saga (xs,ys) = (map (nextStep ns) xs,map (nextStep ns) ys)
           where ns = nextSeason saga

-- | Complete a list of advancement steps.
mapComplete :: StepAdvance c => [AdvancementStep] -> [c]
mapComplete = filterNothing . map completeStepMaybe

-- | Complete a list of advancement steps and split characters and covenants into two lists
mapCompleteSplit :: [AdvancementStep] -> ([Covenant],[Character])
mapCompleteSplit xs = (mapComplete xs,mapComplete xs)

-- |
-- Complete the joint advancement of characters and covenants.
completeJoint :: ([AdvancementStep],[AdvancementStep]) -> ([Covenant],[Character])
completeJoint (xs,ys) = mapCompleteSplit (ys++xs)

-- |
-- Jointly advance characters and covenants.
advJoint :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
advJoint (xs,ys) = (map applyAdv xs, map applyAdv ys)

-- |
-- Find books in the covenants and add to the advancements for characters
-- who use them.
addBooks :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
addBooks (xs,ys) = validateBooks (xs,map (addBook xs') ys)
   where xs' = filterNothing $ map stepSubjectMaybe xs

-- |
-- Validate the use of books.
validateBooks :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
validateBooks (xs,ys) = (xs, f ys)
   where vs = map valGBU $ getBookUse ys
         f [] = []
         f (step:s) 
            | isNothing aa = step:s
            | bookUsed (fromJust aa) == [] = step:s
            | otherwise = step':s
            where (CharStep ch aa) = step
                  step' = CharStep ch (Just $ g (fromJust aa) vs)
         g aa [] = aa
         g aa ((x,v):s) 
           | x `elem` bookUsed aa = g (addValidation [v] aa) s
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
getBookUse :: [AdvancementStep] -> [ ( Book, [Character] ) ]
getBookUse = f5 . f4 . f3 . f2 . f1
   where f1 = map  ( \ x -> (aaBookUsed x,x) )
         f2 = map ( \ (bs, step) -> [ (b,stepSubjectMaybe step) | b <- bs ] ) 
         f3 = sortOn fst . foldl (++) [] 
         f4 = map f4i
         f4i (x,Nothing) = (x,[])
         f4i (x,Just y) = ( x, [y] ) 
         f5 [] = []
         f5 (x:[]) = x:[]
         f5 ((x1,y1):(x2,y2):s) 
             | x1 == x2 = f5 ((x1,y1++y2):s)
             | otherwise = (x1,y1):f5 ((x2,y2):s)

aaBookUsed :: AdvancementStep -> [Book]
aaBookUsed (CharStep _ (Just aa)) = bookUsed aa
aaBookUsed _ = []

-- |
-- Find books in the covenants and add to the advancement of the given
-- character if they use the book.
addBook :: [Covenant] -> AdvancementStep -> AdvancementStep
addBook cvs (CharStep x aa) = CharStep x (fmap (addBook' cov) aa)
   where cov =  findCov x cvs
addBook _ step = step

-- |
-- Find and add books with stats to add to the character advancement.
-- Not implemented yet.
addBook' :: Maybe Covenant -> AugmentedAdvancement -> AugmentedAdvancement
addBook' Nothing y  = y
addBook' (Just cov) y = y { inferredAdv = f bs $ inferredAdv y }
    where u = usesBook y
          bk | isNothing st = [ Nothing | _ <- u ]
             | otherwise = map (findBook (fromJust st)) u
          bs = zip u bk
          st = covenantState cov
          f [] aa = aa
          f ((bid,Nothing):xs) aa = f xs $ addValidation [nobk bid] aa
          f ((_,Just b):xs) aa = f xs $ aa { advBook = b:advBook aa }
          nobk x = ValidationError $ "Book not found (" ++ x ++ ")"

-- |
-- == Covenant and Character Advancement

-- | Generic type for an advancement step for either a covenant or a character.
data AdvancementStep = CovStep Covenant  (Maybe AugCovAdvancement)
                     | CharStep Character (Maybe AugmentedAdvancement)


-- | `StepAdvance` is the class of types to which `AdvancementStep` applies.
class StepAdvance c where
   nextStep :: SeasonTime -> c -> AdvancementStep
   completeStep :: AdvancementStep -> c
   completeStep = fromJust . completeStepMaybe
   completeStepMaybe :: AdvancementStep -> Maybe c
   stepSubject :: AdvancementStep -> c
   stepSubject = fromJust . stepSubjectMaybe
   stepSubjectMaybe :: AdvancementStep -> Maybe c
instance StepAdvance Character where
   nextStep ns ch | fs == [] = CharStep ch Nothing
                 | season adv > ns = CharStep ch Nothing
                 | otherwise = CharStep new  (Just a)
        where a = prepareAdvancement (fromJust st) adv
              st = state ch
              (adv:as) = fs
              fs = futureAdvancement ch
              new = ch { futureAdvancement = as }
   completeStepMaybe (CharStep c Nothing) = Just c 
   completeStepMaybe (CharStep c (Just a)) = Just $ c { pastAdvancement = a:pastAdvancement c }
   completeStepMaybe _ = Nothing
   stepSubjectMaybe (CharStep c _) = Just c 
   stepSubjectMaybe _ = Nothing
instance StepAdvance Covenant where
   nextStep ns cov | fs == [] = CovStep cov Nothing
                 | season adv > ns = CovStep cov Nothing
                 | otherwise = CovStep new  (Just a)
        where a = AugCovAdvancement (Just adv) Nothing
              (adv:as) = fs
              fs = futureCovAdvancement cov
              new = cov { futureCovAdvancement = as }
   completeStepMaybe (CovStep c Nothing) = Just c 
   completeStepMaybe (CovStep c (Just a)) = Just $ c { pastCovAdvancement = a:pastCovAdvancement c }
   completeStepMaybe _ = Nothing
   stepSubjectMaybe (CovStep c _) = Just c
   stepSubjectMaybe _ = Nothing


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
   prepare = prepareCharacter

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

