{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Character.InGame
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  In-Game advancement of Character.
--
--
-----------------------------------------------------------------------------
module ArM.Character.InGame where

import ArM.Types.Harm
import ArM.Types.Advancement
import ArM.Character.Advancement
import ArM.Character.Character
import ArM.Character.Validation
import ArM.Story
import ArM.Trait
import ArM.Processing
import ArM.Helper
import Data.Maybe
import Control.Applicative
import Control.Monad

-- | Initialise `Character` object for advancement
initAdvancement :: SeasonTime -> Character -> Character
initAdvancement t c = c { pastAdvancement = x:pastAdvancement c
                        , futureAdvancement = xs 
                        , state = Just $ f (state c) t
                        }
     where (x,xs) = iaHead t st $ futureAdvancement c
           f Nothing y = defaultCS { charTime = y }
           f (Just s) y = s { charTime = y }
           st = fromMaybe defaultCS $ state c

-- | Empty augmented advancement object with the given time stamp
noAdvT :: SeasonTime -> Augmented Advancement
noAdvT t = Adv a defaultAdvancement []
   where a = defaultAdvancement { advSeason = t }

-- | Empty augmented advancement object
noAdv :: Augmented Advancement
noAdv = noAdvT NoTime

-- | Take the head off the future advancement if the time is right.
iaHead :: SeasonTime -> CharacterState 
       -> [Advancement] -> (Augmented Advancement,[Advancement])
iaHead t _ [] = (noAdvT t,[])
iaHead t st (x:xs) | season x == t = (prepareAdvancement st x,xs)
                   | otherwise = (noAdvT t,xs)

-- | Get the current contracted advancement being processed.
chgCurrentAdv :: Character -> Augmented Advancement
chgCurrentAdv = fromMaybe noAdv . mhead . pastAdvancement

chgStep :: Character -> Character
chgStep ch = setCharacterState st $ setAdvancement aa ch
   where aa' = chgCurrentAdv ch
         st' = fromMaybe defaultCS $ state ch
         (aa,st) = applyAdvancement aa' st'

chgValidate :: Character -> Character
chgValidate ch = updateCharacterAdv (validate ch) ch

chgBook :: SagaState -> Character -> Character
chgBook st ch = updateCharacterAdv (addBook st ch) ch

-- |
-- Find and add books with stats to add to the character advancement.
-- Not implemented yet.
addBook :: SagaState -> Character -> Augmented Advancement -> Augmented Advancement
addBook st ch aa = addBook' st ch aa (mode ca) bkey ikey
    where bs = readsBook ca
          ca = contractAdvancement aa
          bkey = filter isBK bs
          ikey = filter isIK bs
          isBK (BookKey _) = True
          isBK  _ = False
          isIK (ItemKey _) = True
          isIK  _ = False

-- | Auxiliar for `addBook`.
-- Look up possession by possession ID or book ID, adding errors and warnings
-- for inconsistent use. 
addBook' :: SagaState -> Character
          -> Augmented Advancement 
          -> AdvancementType -> [HarmKey] -> [HarmKey] 
          -> Augmented Advancement 
addBook' _ _ aa Reading [] [] = addValidation val aa
    where val = [ ValidationWarning "No book defined for reading" ]
addBook' st ch aa Reading xs [] = addBook2 aa xs item
    where item = join $ fmap (findBook st ch) (mhead xs)
addBook' st ch aa Reading xs ys = addBook2 aa xs item
    where item = join $ fmap (findBook st ch) (mhead ys)
addBook' _ _ aa _ [] [] = aa -- No books - no reading
addBook' _ _ aa _ _ _ = addValidation val aa
    where val = [ ValidationError "Book specified for non-reading season" ]

-- | Find a book in a saga or the character itself.
findBook :: SagaState -> Character -> HarmKey -> Maybe Possession
findBook s c k = ck <|> findBookCh c k
     where f x = findBookCov x k
           ck = ffmap f $ memberOfCovenant s c
           findBookCov _ _ = Nothing
           findBookCh _ _ = Nothing

-- | Apply the function and join, to get rid of nested monads.
ffmap :: Monad m => ( a -> m b ) -> m a -> m b
ffmap f = join . fmap f

-- | Get a character's covenant by looking up the key in the saga.
memberOfCovenant :: SagaState -> Character -> Maybe Covenant
memberOfCovenant saga = ffmap g . fmap CovenantKey . ffmap memberOf . state 
           where g x = harmLookup x saga

addBook2 :: Augmented Advancement -> [ HarmKey ] -> Maybe Possession
         -> Augmented Advancement 
addBook2 aa _ Nothing = addValidation val aa
    where val = [ ValidationError "Book not found" ]
addBook2 aa [] (Just item) = g (primaryXPTrait $ explicitAdv aa') 
    where val1 = [ ValidationError "Tome does not contain texts on the given topic." ]
          val2 = [ ValidationError "Cannot determine topic studied." ]
          f Nothing = addValidation val1 aa'
          f (Just y) = setBook y aa'
          g Nothing = addValidation val2 aa'
          g (Just y) = f $ bookByTopic y $ bookTexts item
          aa' = addRequired item aa
addBook2 aa (x:[]) (Just item) = f $ filter ((x==) . harmKey) (bookTexts item)
    where val = [ ValidationError "Tome does not contain the text specified." ]
          f [] = addValidation val aa'
          f (y:_) = setBook y aa'
          aa' = addRequired item aa
addBook2 aa _ _ = addValidation val aa
    where val = [ ValidationError "More than one book text specified." ]

{-
addBook st ch y = f bs y 
    where u = usesBook $ contractAdvancement y
          bk = findBook st ch u
          bs = zip u bk
          f [] aa = aa
          f ((bid,Nothing):xs) aa = f xs $ addValidation [nobk bid] aa
          f ((_,Just b):xs) aa = f xs $ addB aa b
          nobk x = ValidationError $ "Book not found (" ++ x ++ ")"
          addB ba b = ba { inferredAdv = addB' (inferredAdv ba) b }
          addB' ba b = ba { bookUsed = b:bookUsed ba }

findBook :: SagaState -> Character -> HarmKey -> Maybe Possession
findBook _ _ _ = Nothing
-}

-- ** Convenience functiosn

-- | Find a book by topic from a list
bookByTopic :: TraitKey -> [ Book ] -> Maybe Book
bookByTopic k = mhead . filter (bookHasTopic k)

-- | Check if the book covers a given topic
bookHasTopic :: TraitKey -> Book -> Bool
bookHasTopic k = f . filter ((==k) . topic) . bookStats
     where f [] = False
           f _ = True

-- | Set the `bookRead` field
setBook :: Book -> Augmented Advancement -> Augmented Advancement
setBook y a = a { inferredAdv = (inferredAdv a) { bookRead = Just y } }

-- | Add a required `Possesion` to an advancement
addRequired :: Possession -> Augmented Advancement -> Augmented Advancement
addRequired y a = a { inferredAdv = ia { requires = harmKey y:requires ia } }
   where ia = inferredAdv a
