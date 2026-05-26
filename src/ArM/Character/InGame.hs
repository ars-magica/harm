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
import ArM.Character.Inference
import ArM.Character.Virtues
import ArM.Story
import ArM.Trait
import ArM.Processing
import ArM.GameRules
import ArM.Helper

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import ArM.Debug.Trace

-- | Initialise `Character` object for advancement
initAdvancement :: SeasonTime -> Character -> Character
initAdvancement t c = f $ iaHead t c $ futureAdvancement c
     where f Nothing = c
           f (Just (x,xs)) = c { pastAdvancement = x:pastAdvancement c
                               , futureAdvancement = xs 
                               , charTime = t
                               }

-- | Empty augmented advancement object with the given time stamp
noAdvT :: SeasonTime -> Augmented Advancement
noAdvT t = Adv a defaultAdvancement []
   where a = defaultAdvancement { advSeason = t }

-- | Empty augmented advancement object
noAdv :: Augmented Advancement
noAdv = noAdvT NoTime

-- | Take the head off the future advancement if the time is right.
iaHead :: SeasonTime -> Character 
       -> [Advancement]
       -> Maybe (Augmented Advancement,[Advancement])
iaHead _ _ [] = Nothing
iaHead t st (x:xs) | season x == t = Just (prepareAdvancement st x,xs)
                   | otherwise = Nothing

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: Character -> Advancement -> Augmented Advancement
prepareAdvancement c = sortAdvTraits   -- sort inferred traits
                     . winterEvents c  -- aging
                     . addInference c  -- source quality inference

-- | Get the current contracted advancement being processed.
chgCurrentAdv :: Character -> Augmented Advancement
chgCurrentAdv = fromMaybe noAdv . mhead . pastAdvancement

chgStep :: Character -> Character
chgStep ch' = trace trac $ setAdvancement aa ch
   where aa' = chgCurrentAdv ch'
         (aa,ch) = applyAdvancement aa' ch'
         trac = "chgStep" ++ show (name ch) ++ " " ++ show (season aa)

-- | Infer source quality
chgValidate :: Character -> Character
chgValidate ch = updateCharacterAdv (validate ch) ch
    where validate c = validateXP . inferSQ c

chgBook :: Saga -> Character -> Character
chgBook st ch = updateCharacterAdv (addBook st ch) ch

-- |
-- Find and add books with stats to add to the character advancement.
addBook :: Saga -> Character -> Augmented Advancement -> Augmented Advancement
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
addBook' :: Saga -> Character
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
findBook :: Saga -> Character -> HarmKey -> Maybe Possession
findBook s c k = ( lookupBook k $ characterPossessions c )
             <|> ( ffmap (lookupBook k) $ memberOfCovenant s c )

-- | Apply the function and join, to get rid of nested monads.
ffmap :: Monad m => ( a -> m b ) -> m a -> m b
ffmap f = join . fmap f

-- | Get a character's covenant by looking up the key in the saga.
memberOfCovenant :: Saga -> Character -> Maybe Covenant
memberOfCovenant saga = ffmap g . fmap CovenantKey . memberOf 
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

-- | Infer source qualities
chgSQ :: Character -> Character
chgSQ c = chgSQ' (advancementmode $ chgCurrentAdv c) c

-- | Infer source qualities
chgSQ' :: String -> Character -> Character
chgSQ' "Reading" = updateCharacterAdv readingSQ
chgSQ' _ = id

-- | Check and update source qualities for reading
--
-- May need to
-- 1. deduce topic studied from book
-- 2. deduce SQ from book
-- 3. validate SQ against book
-- 4. validate topic against book
--
-- Not supported at the moment
-- 1. Multiple topics studied
readingSQ :: Augmented Advancement -> Augmented Advancement 
readingSQ aa = readingSQ1 bk aa
    where bk = bookRead $ contractAdvancement aa

-- | Adds warning if no book is specified or continues to `readingSQ2` if it is.
readingSQ1 :: Maybe Book -> Augmented Advancement -> Augmented Advancement 
readingSQ1 Nothing aa = addValidation val aa
    where val = [ ValidationWarning $ "No book for reading season" ]
readingSQ1 (Just bk) aa = readingSQ2 pt (bookStats bk) aa
    where pt = primaryXPProtoTrait $ explicitAdv aa

-- | Check if there are bookstats and a trait being learnt.
-- 1. If there are no book stats, a warning is returned.
-- 2. If there is not trait being learnt, it continues to `readingSQaddPT`,
--    adding a warning if there are more than one set of book stats.
-- 3. If there is a trait to learn, it continues to `readingSQstat`
readingSQ2 :: Maybe ProtoTrait -> [BookStats] -> Augmented Advancement 
           -> Augmented Advancement 
readingSQ2 _ [] = addValidation val
    where val = [ ValidationWarning $ "Book has no stats" ]

readingSQ2 Nothing (x:[]) = readingSQaddPT x
readingSQ2 Nothing (x:_) = readingSQaddPT x . addValidation val
    where val = [ ValidationWarning $ "Book has several book stats; using the first one." ]
readingSQ2 (Just pt) xs = readingSQstat pt xs

-- | Identify correct book stats for the trait being learnt
readingSQstat :: ProtoTrait -> [BookStats] -> Augmented Advancement 
           -> Augmented Advancement 
readingSQstat pt xs = trace ("readingSQstat "++show (stat,show $ protoTrait pt,
                            (fmap show $ fmap topic $ mhead xs)))
                    $ trace (show $ mhead xs)
                    $ f stat
    where stat = find ( (==protoTrait pt) . topic ) xs
          f Nothing = addValidation val
          f (Just bk) = readingSQ4 bk
          val = [ ValidationError $ "Book does not cover the topic " ++ show (protoTrait pt) ]

-- | Infer source quality from book stats
readingSQ4 :: BookStats -> Augmented Advancement -> Augmented Advancement 
readingSQ4 b aa = aa { inferredAdv = f $ inferredAdv aa }
    where f x = x { sourceQuality = quality b } 

-- | Infer `ProtoTrait` for learning from book stats.
readingSQaddPT :: BookStats -> Augmented Advancement -> Augmented Advancement 
readingSQaddPT bk aa = aa { inferredAdv = ia { changes = pt:changes ia } }
    where pt = defaultPT { protoTrait = topic bk }
          ia = inferredAdv aa
          
-- | Check if converge to are reread
chgRepeat :: Character -> Character
chgRepeat ch = f bk ch
     where f Nothing = id
           f (Just b) | isTractatus b = g $ find ( (==(harmKey b)) . harmKey ) bs
                      | otherwise = id
           g Nothing = id
           g (Just b) = addCharacterValidation (val b)
           val b = [ ValidationError $ "Rereading tractatus: " ++ name b ]
           bk = bookRead $ contractAdvancement $ chgCurrentAdv ch
           bs = filter isTractatus $ chgBooksRead ch


-- | List of books that the character has read, with possible repetitions.
chgBooksRead :: Character -> [ Book ]
chgBooksRead = filterNothing . map ( bookRead . contractAdvancement ) 
             . mtail . pastAdvancement


-- | Infer source quality.
--
-- This includes
-- 1. BonusSQ from virtues and flaws, calculated by 'vfBonusSQ'.
-- 2. Exposure SQ calculated by 'getSQ'.
--
-- Source quality from reading is calculated during book management.
--
-- TODO
-- 1. Infer SQ for taught from teacher
-- 2 Infer SQ for adventure from covenant
inferSQ :: Character -> Augmented Advancement -> Augmented Advancement
inferSQ cs ad = ad { inferredAdv = aa { sourceQuality = sq, bonusSQ = vfBonusSQ vf ad } }
        where vf = vfList cs
              (sq,_) = getSQ ad
              aa = inferredAdv ad

-- | Get source quality.  Auxiliary for `inferSQ`.
getSQ :: Augmented Advancement -> (Maybe XPType,Maybe Int)
getSQ a | isExposure a = (Just 2,Nothing)
        | (mode $ explicitAdv a) == Practice 
          && (isNothing $ sourceQuality $ explicitAdv a)
          = (Just 4,Nothing)
        | otherwise = (Nothing,Nothing)
