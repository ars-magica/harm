{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Saga.Adv
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  In-game Advancement of Sagas, Covenants, and Characters.
--
-- Where pre-game advancement can be done independently for each covenant
-- and each character, in-game advancement is interdependent.  Conflicts
-- over books and other resources must be resolved, and source qualities
-- may be inferred from other entitites.
--
-- In-game advancement is managed through saga advancement, and divided into
-- three different steps.  It depends on the `StepAdvance` class comprising
-- `Covenant` and `Character`.  The three functions of the class represent
-- the three steps of the advancement process
-- + nextStep
-- + applyStep
-- + completeStep
-- The Saga functions can interleave these steps with global inference and
-- validation.
--
--
-----------------------------------------------------------------------------
module ArM.Saga.Adv ( advanceSaga 
                       , Advance(..)
                       , StepAdvance(..)
                       , Validation(..)
                       ) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Character
import ArM.Covenant
import ArM.Story
import ArM.Trait
import ArM.Types.Harm
import ArM.Saga.DB
import ArM.Helper

-- |
-- * Types

-- |
-- The Advance class represents objects which change state from
-- season to season.
--
-- Normally only Saga objects should be advanced explicitly.
-- Other instances exist for internal use in the Saga instance.
class Timed a => Advance a where

    -- | Next season - if this is undefined (e.g. at GameStart),
    -- the time of the next advancement is returned.
    nextSeason :: a -> SeasonTime
    nextSeason c = f $ season c 
       where ns = nextAdvancement c
             f GameStart = ns
             f NoTime = ns
             f ssn = seasonNext ssn

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

-- | The saga can be advanced with the same class methods as
-- characters and covenants.  When the saga advances, all its
-- characters and covenants advance accordingly.
instance Advance Saga where
   nextAdvancement saga  = min charnext covnext 
      where charnext = foldl min NoTime [ nextAdvancement x | x <- characters st ]
            covnext = foldl min NoTime [ nextAdvancement x | x <- covenants st ]
            st = sagaState saga

-- |
-- The `Advance` instance is very similar to that of `Character`, but has to
-- be implemented separately to account for different advancement classes.
instance Advance Covenant where
   nextAdvancement c = f $ futureCovAdvancement c
       where f [] = NoTime
             f (x:_) = caSeason x
   prepare = covGen

instance Advance Character where
   nextAdvancement = f . futureAdvancement
       where f [] = NoTime
             f (x:_) = season x
   prepare = prepareCharacter

-- OK above

data InterimSaga = InterimSaga Saga

-- | `StepAdvance` is the class of types to which `AdvancementStep` applies.
class StepAdvance c where
   -- | Create the next `AdvancementStep` object for a character or covenant.
   nextStep :: SeasonTime -> c -> AdvancementStep
   -- | Clean up the advancement step. This should only be applied after
   -- `applyStep`.
   completeStep :: AdvancementStep -> c
   completeStep = fromJust . completeStepMaybe
   completeStepMaybe :: AdvancementStep -> Maybe c
   -- | Get the subjct (i.e. covenant or character) from the object.
   -- Note that this returns an error when the type of the `StepAdvance`
   -- object does not match the actual contents.
   stepSubject :: AdvancementStep -> c
   stepSubject = fromJust . stepSubjectMaybe
   -- | Get the subjct (i.e. covenant or character) from the object,
   -- returning Nothing if the constituent subject does not match
   -- the required type.
   stepSubjectMaybe :: AdvancementStep -> Maybe c
   -- | Get the advancement fromn a `AdvancementStep` object.
class StepAdvanceAdv c where
   -- | Get the advancement fromn a `AdvancementStep` object.
   stepAdvancement :: AdvancementStep -> Maybe (Augmented c)
instance StepAdvanceAdv CovAdvancement where
   stepAdvancement (CovStep _ a) = a
   stepAdvancement _ = Nothing
instance StepAdvanceAdv Advancement where
   stepAdvancement (CharStep _ a) = a
   stepAdvancement _ = Nothing

instance StepAdvance Character where
   nextStep ns ch = nextStep' fs
        where fs = futureAdvancement ch
              nextStep' [] = CharStep ch Nothing
              nextStep' (adv:_)  
                 | season adv > ns = CharStep ch Nothing
                 | otherwise = CharStep new  (Just $ prepareAdvancement (fromJust st) adv)
              new = ch { futureAdvancement = mtail fs }
              st = state ch
   completeStepMaybe (CharStep c Nothing) = Just c 
   completeStepMaybe (CharStep c (Just a)) = Just $ c { pastAdvancement = a:pastAdvancement c }
   completeStepMaybe _ = Nothing
   stepSubjectMaybe (CharStep c _) = Just c 
   stepSubjectMaybe _ = Nothing


instance StepAdvance Covenant where
   nextStep ns cov = nextStep' fs
        where fs = futureCovAdvancement cov
              nextStep' [] = CovStep cov Nothing
              nextStep' (adv:_)  
                 | season adv > ns = CovStep cov Nothing
                 | otherwise = CovStep new  (Just $ Adv adv noCovAdvancement [])
              new = cov { futureCovAdvancement = mtail fs }
   completeStepMaybe (CovStep c Nothing) = Just c 
   completeStepMaybe (CovStep c (Just a)) = 
        Just $ c { pastCovAdvancement = a:pastCovAdvancement c }
   completeStepMaybe _ = Nothing
   stepSubjectMaybe (CovStep c _) = Just c
   stepSubjectMaybe _ = Nothing


-- |
-- ** Covenant and Character Advancement

-- | Generic type for an advancement step for either a covenant or a character.
data AdvancementStep = CovStep Covenant  (Maybe (Augmented CovAdvancement))
                     | CharStep Character (Maybe (Augmented Advancement))

instance BookDB AdvancementStep where
   bookLookup (CovStep c _) k = bookLookup c k
   bookLookup _ _ = Nothing

-- |
-- * Saga Advancement

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
advanceSaga saga = reverse $ saga:advanceSaga' (advSeasons saga) saga

advanceSaga' :: [SeasonTime] -> Saga -> [ Saga ]
advanceSaga' [] _ = []
advanceSaga' (t:ts) saga0 = n:advanceSaga' ts n
    where n = f t saga0
          f ssn saga | NoTime == nextSeason saga = saga 
                     | ssn < nextSeason saga = saga 
                     | otherwise = f ssn $ stepSaga saga

-- |
-- * Joint Character and Covenant Advancement

-- |
-- Advance listed covenants and characters one season forward.
-- The advancement happens jointly, with several passes, to resolve
-- inter-dependencies.
jointAdvance :: Saga   -- ^ Saga reference, passed to know what the next season is
     -> ([Covenant],[Character]) -- ^ Lists of prior covenants and characters
     -> ([Covenant],[Character]) -- ^ Lists of future covenants and characters
jointAdvance saga = completeJoint . validateStep . validateBookUse . advJoint . nextJoint saga

-- |
-- Get the next advancements, preparing for joint advancement
nextJoint :: Saga -> ([Covenant],[Character]) -> ([AdvancementStep],[AdvancementStep]) 
nextJoint saga (xs,ys) = (map (nextStep ns) xs,map (nextStep ns) ys)
           where ns = nextSeason saga
-- |
-- Jointly advance characters and covenants.
advJoint :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
advJoint (xs,ys) = (map applyStep xs, map applyStep ys)

-- | Validate individual characters.  Nothing is currently done for
-- covenants.
validateStep :: ([AdvancementStep],[AdvancementStep]) 
             -> ([AdvancementStep],[AdvancementStep]) 
validateStep (xs,ys) = (map f xs,map f ys)
    where f (CharStep c (Just a)) = CharStep c $ Just $ validate c a
          f step = step

-- | Apply the next advancement step.
--
-- The main process is defined by the `applyAdvancement` function from
-- `ArM.Char.Advancement`
applyStep :: AdvancementStep -> AdvancementStep
applyStep (CharStep c Nothing) = (CharStep c Nothing) 
applyStep (CharStep c (Just aa)) = (CharStep c' (Just a')) 
       where (a',st') = applyAdvancement aa st
             c' = c { state = Just st' }
             st = fromMaybe defaultCS $ state c
applyStep (CovStep c Nothing) = (CovStep c Nothing) 
applyStep (CovStep c (Just aa')) = (CovStep c' (Just aa')) 
     where aa = contractAdvancement aa'
           c' = c { covenantState = Just $ stepCovState (fs st) aa }
           fs x = x { covTime = caSeason aa }
           st = fromMaybe defaultCovState $ covenantState c

-- |
-- Complete the joint advancement of characters and covenants.
completeJoint :: ([AdvancementStep],[AdvancementStep]) -> ([Covenant],[Character])
completeJoint (xs,ys) = mapCompleteSplit (ys++xs)

-- | Complete a list of advancement steps and split characters 
-- and covenants into two lists
mapCompleteSplit :: [AdvancementStep] -> ([Covenant],[Character])
mapCompleteSplit xs = (mapComplete xs,mapComplete xs)

-- | Polymorphic extraction of Covenant/Character objects from
-- AdvanceStep.  This is an auxiliary for `mapCompleteSplit`.
mapComplete :: StepAdvance a => [AdvancementStep] -> [a]
mapComplete = filterNothing . map completeStepMaybe


-- |
-- * Book Management

-- $books
-- The book validation consists of several steps.
-- 1. `addBooks` add book objects using the key recorded.
--    A ValidationError is created if the book is not found.
-- 2. `bookCollision` checks for conflicting use requests
-- 3. **TODO** `bookSQ` checks the source quality, if the book is read
-- 4. **TODO** `bookRepeat` checks for repeat reading of tractatus
-- 5. **TODO** create new books on copying
-- 5. **TODO** create new books on authoring

covSort :: ([AdvancementStep],[AdvancementStep]) 
        -> [(AdvancementStep,[AdvancementStep])]
covSort (xs,ys) =

-- | Validation and inference concerning books.
validateBookUse :: ([AdvancementStep],[AdvancementStep]) 
                -> ([AdvancementStep],[AdvancementStep]) 
validateBookUse = bookRepeat . bookSQ . bookCollision . addBooks

-- |
-- ** The addBook step

-- Find books in the covenants and add to the advancements for characters
-- who use them.
--
-- Note that books are currently only taken from the character's covenant.
-- This will have to be extended to allow reading as a guest, and books
-- borrowed from other characters or covenants.
addBooks :: ([AdvancementStep],[AdvancementStep]) 
         -> ([AdvancementStep],[AdvancementStep]) 
addBooks (xs,ys) = (xs,map (addBook covs) ys)
   where covs = filterNothing $ map stepSubjectMaybe xs
--
-- |
-- Find books in the covenants and add to the advancement of the given
-- character if they use the book.
addBook :: [Covenant] -> AdvancementStep -> AdvancementStep
addBook cvs (CharStep x aa) = CharStep x (fmap (addBook' cov) aa)
   where cov =  findCov x cvs
addBook _ step = step

-- |
-- Find and add books with stats to add to the character advancement.
-- This step in the chain will split book and possession keys
addBook' :: Maybe Covenant -> Augmented Advancement -> Augmented Advancement
addBook' cov y = addBook'' cov y (mode $ contractAdvancement y) bkey ikey 
    where bkey = filter isBK $ readsBook $ contractAdvancement y
          ikey = filter isIK $ readsBook $ contractAdvancement y
          isBK (BookKey _) = True
          isBK  _ = False
          isIK (ItemKey _) = True
          isIK  _ = False

-- | Auxiliar for `addBook;`.
-- Look up possession by possession ID or book ID, adding errors and warnings
-- for inconsistent use. 
addBook'' :: Maybe Covenant -> Augmented Advancement 
          -> AdvancementType -> [HarmKey] -> [HarmKey] 
          -> Augmented Advancement 
addBook'' _ aa Reading [] [] = addValidation val aa
    where val = [ ValidationWarning "No book defined for reading" ]
addBook'' Nothing aa Reading _ _ = addValidation val aa
    where val = [ ValidationWarning "Book not found (no covenant)" ]
addBook'' (Just cov) aa Reading xs [] = addBook3 aa xs item
    where item = tmp $ fmap (bookLookup cov) (mhead xs)
addBook'' (Just cov) aa Reading xs ys = addBook3 aa xs item
    where item = tmp $ fmap (bookLookup cov) (mhead ys)
addBook'' _ aa _ [] [] = aa
addBook'' _ aa _ _ _ = addValidation val aa
    where val = [ ValidationError "Book specified for non-reading season" ]

tmp (Just (Just x)) = Just x
tmp _ = Nothing

-- | Auxliary for `addBook''`.  Process the Maybe Book as looked up by
-- `addBook''`.
addBook3 :: Augmented Advancement 
          -> [HarmKey] -> Maybe Possession
          -> Augmented Advancement 
addBook3 aa _ Nothing = addValidation val aa
    where val = [ ValidationError "Book not found" ]
addBook3 aa [] (Just item) = g (primaryXPTrait $ explicitAdv aa') 
    where val1 = [ ValidationError "Tome does not contain texts on the given topic." ]
          val2 = [ ValidationError "Cannot determine topic studied." ]
          f Nothing = addValidation val1 aa'
          f (Just y) = setBook y aa'
          g Nothing = addValidation val2 aa'
          g (Just y) = f $ bookByTopic y $ bookTexts item
          aa' = addRequired item aa
addBook3 aa (x:[]) (Just item) = f $ filter ((x==) . harmKey) (bookTexts item)
    where val = [ ValidationError "Tome does not contain the text specified." ]
          f [] = addValidation val aa'
          f (y:_) = setBook y aa'
          aa' = addRequired item aa
addBook3 aa _ _ = addValidation val aa
    where val = [ ValidationError "More than one book text specified." ]

bookByTopic :: TraitKey -> [ Book ] -> Maybe Book
bookByTopic k = mhead . filter (bookHasTopic k)

bookHasTopic :: TraitKey -> Book -> Bool
bookHasTopic k = f . filter ((==k) . topic) . bookStats
     where f [] = False
           f _ = True

setBook :: Book -> Augmented Advancement -> Augmented Advancement
setBook y a = a { inferredAdv = (inferredAdv a) { bookRead = Just y } }

addRequired :: Possession -> Augmented Advancement -> Augmented Advancement
addRequired y a = a { inferredAdv = ia { requires = harmKey y:requires ia } }
   where ia = inferredAdv a

-- |
-- ** Book Collisions

-- | Add validation errors to Character advancements where a book
-- is oversubscribed.
bookCollision :: ([AdvancementStep],[AdvancementStep]) 
              -> ([AdvancementStep],[AdvancementStep]) 
bookCollision (cvs,chs) = (cvs,map (itemConflict cbs) chs)
    where cbs = stepCountItems chs

-- | Count uses of books in an advancement step
stepCountItems :: [AdvancementStep]  -- ^ List of character advancement steps for one season
               -> [(HarmKey,Int)] -- ^ List of books with number of users
stepCountItems = countRepetitions . stepItems

-- | Get a list of books used in the seqason
stepItems :: [AdvancementStep] -> [HarmKey]
stepItems = foldl (++) [] . map ( requires . contractAdvancement ) 
            . filterNothing . map stepAdvancement

-- | Add validation errors to one Character advancement, given a list
-- of counted book uses.
itemConflict :: [(HarmKey,Int)] -> AdvancementStep -> AdvancementStep
itemConflict bcs (CharStep ch (Just ad)) = CharStep ch (Just ad')
    where bks = requires  $ contractAdvancement ad
          ad' = addValidation vs ad
          vs = bkCollisions bcs bks
itemConflict _ step = step

-- | Check for oversubscribed books reporting as a list of Validation
-- objects.
bkCollisions :: [(HarmKey,Int)]  -- ^ List of books and numbers of subscribers.
             -> [HarmKey]        -- ^ List of books to check for oversubscription.
             -> [Validation]  -- ^ Verification or error for each book checked.
bkCollisions bcs bks = f bcs $ sort bks 
   where  f [] _ = []
          f _ [] = []
          f (c:cs) (b:bs) | fst c < b = f cs (b:bs)
                          | fst c > b = f (c:cs) bs
                          | otherwise = val (snd c) b:f cs bs
          val c b | count b < c = ValidationError $ show b ++ " is oversubscribed"
                  | otherwise = Validated $ show b ++ " is available."





-- |
-- ** Check for rereading

-- | Get a list of all books read by a character
allBooksRead :: Character -> [ Book ]
allBooksRead = filterNothing . map ( bookRead . contractAdvancement ) . pastAdvancement 

-- |
-- ** Other Book steps

-- | Check if a tractatus is read for the second time
bookRepeat :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
bookRepeat = id

{-
bookRepeat (xs,ys) = (xs, map (bookRepeat' xs) ys)

-- | Check a single character to see if they reread a tractatus 
bookRepeat' :: [AdvancementStep] -> AdvancementStep -> AdvancementStep
bookRepeat' db (CharStep c (Just ad)) | md == Reading
      = valRepeat $ CharStep c (Just $ valRead $ setRead db ad)
   where md = mode $ contractAdvancement ad
bookRepeat' _ step = step
-}

valRepeat :: AdvancementStep -> AdvancementStep
valRepeat (CharStep c Nothing) = (CharStep c Nothing)
valRepeat (CharStep c (Just ad)) = (CharStep c (Just ad'))
   where -- bks = sort $ filterNothing $ map ( maybeHead . readBook ) ads
         -- bk = readBook ad'
         ad' = ad
         -- ads = pastAdvancement c
valRepeat step = step

valRead :: Augmented Advancement -> Augmented Advancement 
valRead ad = g ad $ readsBook $ contractAdvancement ad
    where g x [] = addValidation nobk x
          g x [_] = x
          g x (_:_:_) = addValidation xbk x
          nobk = [ ValidationError $ "Missing book for reading season" ]
          xbk = [ ValidationError $ "Superfluous books for reading season" ]
          

{-
bookRepeat'' xs x = f x
    where bs = filterNothing $ map getBookRead xs
          b = fmap originalKey $ getBookRead x
          f Nothing = x
          f (Just b) | isTractatus b = x
                     | otherwise = find (==b) bs
                     -- addValidation err x
                     -- | otherwise = addValidation err x
          bid = fromMaybe "" $ fmap bookID b
          err = ValidationError $ "Tractatus " ++ bid ++ " is read for the second time."

-}

-- | Add and validate source quality on reading advancements
bookSQ :: ([AdvancementStep],[AdvancementStep]) -> ([AdvancementStep],[AdvancementStep]) 
bookSQ (xs,ys) = (xs,map bookSQ' ys)

bookSQ' :: AdvancementStep -> AdvancementStep
bookSQ' (CharStep c (Just ad)) = CharStep c (Just $ bookAdvSQ ad)
bookSQ' step = step
bookAdvSQ :: Augmented Advancement -> Augmented Advancement
bookAdvSQ = id


