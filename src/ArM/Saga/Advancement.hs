{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Saga.Advancement
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
-- The Saga functions can interleave these steps with global inference and
-- validation.
--
--
-----------------------------------------------------------------------------
module ArM.Saga.Advancement ( advanceSaga 
                       , Advance(..)
                       , Validation(..)
                       ) where

import Data.Maybe 
import Data.List 
import qualified Data.Map as M

import ArM.Saga.Saga
import ArM.Character
import ArM.Covenant
import ArM.Processing
import ArM.Trait
import ArM.Story
import ArM.Types.Harm
import ArM.Helper

import ArM.Debug.Trace

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

-- | The saga can be advanced with the same class methods as
-- characters and covenants.  When the saga advances, all its
-- characters and covenants advance accordingly.
instance Advance Saga where
   nextAdvancement saga  = min charnext covnext 
      where charnext = M.foldr min NoTime $ M.map nextAdvancement $ characters st 
            covnext = M.foldr min NoTime $ M.map nextAdvancement $ covenants st 
            -- covnext = M.foldr min NoTime [ nextAdvancement x | x <- covenants st ]
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

-- |
-- * Saga Advancement
--
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

-- | Map an advancement function on every covenant
covenMapS :: ( SagaState -> Covenant -> Covenant ) ->  SagaState -> SagaState
covenMapS f st = covenMap (f st) st
-- | Map an advancement function on every covenant
covenMap :: ( Covenant -> Covenant ) ->  SagaState -> SagaState
covenMap f st = st { covenants = M.map f $ covenants st }
-- | Map an advancement function on every covenant
charMap :: ( Character -> Character ) ->  SagaState -> SagaState
charMap f st = st { characters = M.map f $ characters st }

-- | 
-- ** Advancement step

-- | Advance the saga forward by one season.
stepSaga :: Saga -> Saga
stepSaga saga = saga { sagaState = stepSagaState $ f $ sagaState saga }
     where f x = x { seasonTime = nextSeason saga }

-- | Advance the sagaState forward by one season.
stepSagaState :: SagaState -> SagaState
stepSagaState = stepVal        -- validate characters and covenants individually
              . stepAdv        -- advance characters and covenants individually
              . stepBook       -- Look up books and check for conflict (joint step) 
              . stepMembership -- Update characters to reflect covenant affiliation
              . stepCovenFolk  -- Initialise covenants and update covenfolk
              . stepInit       -- Initialise individual characters for advancement

-- | Initialise characters
stepInit :: SagaState -> SagaState
stepInit st = st { characters = M.map f $ characters st }
     where f = initAdvancement (season st) 

-- | Initialise covenants and update covenfolk
stepCovenFolk :: SagaState -> SagaState
stepCovenFolk st = st { covenants = M.map f $ covenants st }
     where f = cvgCovenFolk .  initCovAdvancement (season st) 


stepMembership :: SagaState -> SagaState
stepMembership st = st { characters = ch }
    where ch = updateMembership ch' $ M.elems $ covenants st
          clear x = x { state = fmap f (state x) }
          f x = x { memberOf = Nothing } 
          ch' = M.map clear $ characters st

-- | For each covenant in the list, update all its covenfolk with new `memberOf`
-- value.
updateMembership :: M.Map String Character -> [Covenant] -> M.Map String Character
updateMembership ch [] = ch
updateMembership ch (x:xs) = updateMembership (updateMembership' ch y i) xs
      where y = fromMaybe [] $ fmap covenFolkID $ covenantState x
            i = harmKey x

updateMembership' :: M.Map String Character -> [ HarmKey ] -> HarmKey 
                  -> M.Map String Character
updateMembership' ch [] _ = ch
updateMembership' ch (CharacterKey k:ks) ck = updateMembership' ch' ks ck
    where ch' = M.adjust (updateCharMembership ck) k ch
updateMembership' _ (_:ks) ck = updateMembership' ch'  ks ck
    where ch' = error "Non-character key for covenfolk"
    -- The error could have been put as a Validation, but then we would 
    -- have to rewrite the functions to have a covenant to put it into.

-- | Update the `memberOf` attribute of a `Character` object.
updateCharMembership :: HarmKey -> Character -> Character
updateCharMembership (CovenantKey k) ch 
    | isNothing m = updateCharacterState f ch
    | otherwise   =  addCharacterValidation val ch
    where val = [ValidationError $ "Character is member of two covenants: "
                ++ k ++ " and " ++ fromJust m  ++ "."
                ]
          m = ff $ fmap memberOf $ state ch
          f x = x { memberOf = Just k }
          ff (Just (Just x)) = Just x  
          ff _ = Nothing
updateCharMembership _ ch = addCharacterValidation val ch
   where val = [ValidationError "Programming error: Non-covenant key for character."]

stepAdv :: SagaState -> SagaState
stepAdv = stepAdvChar . stepAdvCov

stepAdvChar :: SagaState -> SagaState
stepAdvChar = charMap chgStep 
stepAdvCov :: SagaState -> SagaState
stepAdvCov = covenMap cvgStep 


stepVal :: SagaState -> SagaState
stepVal = stepValChar . stepValCov

stepValChar :: SagaState -> SagaState
stepValChar = charMap chgValidate 
stepValCov :: SagaState -> SagaState
stepValCov = trace "Not implemented: stepValCov"


-- |
-- * Book Management

-- $books
-- The book validation consists of several steps.
-- 1. `addBooks` add book objects to the character for each book key.
--    A ValidationError is created if the book is not found.
-- 2. `bookCollision` checks for conflicting use requests
-- 3. **TODO** `bookSQ` checks the source quality, if the book is read
-- 4. **TODO** `bookRepeat` checks for repeat reading of tractatus
-- 5. **TODO** create new books on copying
-- 5. **TODO** create new books on authoring

-- | Validation and inference concerning books.
stepBook :: SagaState -> SagaState
stepBook = bookRepeat . bookSQ . bookCollision . addBooks

-- |
-- Find books in the covenants or character and add to the advancements
-- for characters who use them.
--
-- Note that books are currently only taken from the character's covenant.
-- This will have to be extended to allow reading as a guest, and books
-- borrowed from other characters or covenants.
addBooks :: SagaState -> SagaState
addBooks st = st { characters = M.map (chgBook st) $ characters st }


-- | Add validation errors to Character advancements where a book
-- is oversubscribed.
bookCollision :: SagaState -> SagaState 
bookCollision = covenMapS bookCollisionCov 

bookCollisionCov :: SagaState -> Covenant -> Covenant
bookCollisionCov st cv = addCovenantValidation (bkCollisions rq av) cv
      where rq = countRequires st cv
            av = countAvailable cv

-- | Count uses of books in an advancement step
countRequires :: SagaState
              -> Covenant  -- ^ List of character advancement steps for one season
              -> [(HarmKey,Int)]       -- ^ List of books with number of users
countRequires s = countRepetitions . listRequires s

-- | Count available books
countAvailable :: Covenant  -- ^ List of character advancement steps for one season
              -> [Possession]       -- ^ List of books with number of users
countAvailable = fromMaybe [] . fmap possessions . covenantState


-- | Get a list of books used in the seqason
listRequires :: SagaState -> Covenant -> [HarmKey]
listRequires saga = sort . foldl (++) [] 
                  . map ( requires . contractAdvancement . chgCurrentAdv ) 
                  . fromMaybe [] . fmap ( covenFolk saga ) . covenantState



-- | Check for oversubscribed books reporting as a list of Validation
-- objects.
bkCollisions :: [(HarmKey,Int)]  -- ^ List of books and numbers of subscribers.
             -> [Possession]     -- ^ List of available books 
             -> [Validation]  -- ^ Verification or error for each book checked.
bkCollisions bcs bks = f bcs $ sort bks 
   where  f [] _ = []
          f _ [] = []
          f (c:cs) (b:bs) | fst c < harmKey b = f cs (b:bs)
                          | fst c > harmKey b = f (c:cs) bs
                          | otherwise = val c b:f cs bs
          val c b | count b < snd c = ValidationError $ name b ++ " is oversubscribed"
                  | otherwise = Validated $ name b ++ " is available."


-- | Add and validate source quality on reading advancements
bookSQ :: SagaState -> SagaState
bookSQ = charMap chgSQ

-- | Check if a tractatus is read for the second time
bookRepeat :: SagaState -> SagaState 
bookRepeat = charMap chgRepeat

-- |
-- ** Other Book steps

{-

-- | Check a single character to see if they reread a tractatus 
bookRepeat' :: [AdvancementStep] -> AdvancementStep -> AdvancementStep
bookRepeat' db (CharStep c (Just ad)) | md == Reading
      = valRepeat $ CharStep c (Just $ valRead $ setRead db ad)
   where md = mode $ contractAdvancement ad
bookRepeat' _ step = step

valRepeat :: AdvancementStep -> AdvancementStep
valRepeat (CharStep c Nothing) = (CharStep c Nothing)
valRepeat (CharStep c (Just ad)) = (CharStep c (Just ad'))
   where -- bks = sort $ filterNothing $ map ( mhead . readBook ) ads
         -- bk = readBook ad'
         ad' = ad
         -- ads = pastAdvancement c
valRepeat step = step

valRead :: Augmented Advancement -> Augmented Advancement 
valRead ad = g ad $ readBook $ contractAdvancement ad
    where g x [] = addValidation nobk x
          g x [_] = x
          g x (_:_:_) = addValidation xbk x
          nobk = [ ValidationError $ "Missing book for reading season" ]
          xbk = [ ValidationError $ "Superfluous books for reading season" ]
          
-}

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

