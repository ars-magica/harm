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
import ArM.Char.CharacterSheet
import ArM.Char.Virtues
import ArM.Char.Inference
import ArM.Char.Validation
import ArM.Types.ProtoTrait
-- import ArM.Types.Trait
import ArM.Types.Covenant
-- import ArM.Types.Character
import ArM.Types.Library
import ArM.Types
import ArM.Types.Saga
import ArM.Helper
import ArM.GameRules

import ArM.Debug.Trace

-- |
-- = Advancement

-- |
-- The Advance class represents objects which change state from
-- season to season.
class Timed a => Advance a where
    -- | Advance the character until after the given time.
    advance :: SeasonTime -> a -> a
    -- | Advance the character forward by the first advancement in
    -- the plan, regardless of the season.
    step :: a -> a
    -- | Advance the character one season forward
    stepIf :: SeasonTime -> a -> a
    stepIf _ = step
    -- | Time of the next advancement of the character.
    nextSeason :: a -> SeasonTime
    nextSeason c | ssn == GameStart = ns
                 | ssn == NoTime = ns
                 | otherwise = seasonNext ssn
       where ssn = season c
             ns = nextAdvancement c
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
   -- advance :: SeasonTime -> a -> a
   advance t saga 
      | NoTime == ns = saga 
      | t < ns = saga 
      | otherwise = advance t $ step saga
     where ns = nextSeason saga

   -- step :: a -> a
   step saga = saga { sagaState = st' }
     where st' = st { stateTitle = stateTitle st 
                    , seasonTime = ns
                    , covenants = cov
                    , characters = ch
                    }
           st = sagaState saga
           (cov,ch) = jointAdvance saga ((covenants st),(characters st))
           ns = nextSeason saga

   nextAdvancement saga  = min charnext covnext 
      where charnext = foldl min NoTime [ nextAdvancement x | x <- characters st ]
            covnext = foldl min NoTime [ nextAdvancement x | x <- covenants st ]
            st = sagaState saga

-- | Advance the Saga according to timestamp in the SagaFile.
advanceSaga :: Saga -> [ Saga ]
advanceSaga saga = reverse $ saga:advanceSaga' ts saga
   where ts = seasons $ sagaFile saga

advanceSaga' :: [SeasonTime] -> Saga -> [ Saga ]
advanceSaga' [] _ = []
advanceSaga' (t:ts) x = trace ("adv> " ++ show t) $ n:advanceSaga' ts n
    where n = advance t x

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
   advance ct c | isNothing (covenantState c) = trace "need prepare" $ advance ct $ prepare c
                | ct < ct' =  c
                | otherwise =  advance ct $ step c 
            where ct' =  nextSeason c

   stepIf ns = trace ("stepIf (Cov): "++show ns) $ completeCovAdv . applyCovAdv . nextCovAdv ns

   step cov = completeCovAdv $ applyCovAdv (new,Just y')
            where (y:ys) = futureCovAdvancement cov
                  y' = prepareCovAdvancement y
                  new = cov { futureCovAdvancement = ys }
   nextAdvancement c = f $ futureCovAdvancement c
       where f [] = NoTime
             f (x:_) = caSeason x
   prepare x = trace "prepare Covenant" $ f x
        where f y | isNothing (covenantState y) = y { covenantState = Just defaultCovState }
                  | otherwise = y 

-- | CovenFolk joining according to the augmented covenant advancement.
joiningAug :: AugCovAdvancement -> [HarmKey]
joiningAug (AugCovAdvancement a b) = a' ++ b'
    where a' = fromMaybe [] $ fmap joining a
          b' = fromMaybe [] $ fmap joining b

-- | CovenFolk leaving according to the augmented covenant advancement.
leavingAug :: AugCovAdvancement -> [HarmKey]
leavingAug (AugCovAdvancement a b) = a' ++ b'
    where a' = fromMaybe [] $ fmap leaving a
          b' = fromMaybe [] $ fmap leaving b

-- | Get the season of the augmented covenant advancement.
-- This is taken from the explicit advancement if available, and
-- the inferred advancement otherwise.
caSeasonAug :: AugCovAdvancement -> SeasonTime
caSeasonAug (AugCovAdvancement a b) = fromMaybe b' $ fmap caSeason a
    where b' = fromMaybe NoTime $ fmap caSeason b

-- | Apply covenant advancement
applyCovAdv :: (Covenant,Maybe AugCovAdvancement)
         -> (Covenant,Maybe AugCovAdvancement)
applyCovAdv (c,Nothing) = (c,Nothing)
applyCovAdv (c,Just a) = (c',Just a)
    -- where (a',st') = applyCovAdvancement a st
    where st' = st { covTime = caSeasonAug a, covenFolkID = cid }
          c' = c { covenantState = Just st' }
          st = fromMaybe defaultCovState $ covenantState c
          cid1 = sort $ joiningAug a ++ covenFolkID st 
          cid = cid1 -= ( sort $ leavingAug a )

-- |
-- == Character Advancement

-- |
-- The implementation of character advancement depends on three auxiliary functions.
-- + `prepareCharacter` advances the character from a Nothing state to Game Start.
-- + `prepareAdvancement` augments the advancement object with limits and inference
-- + `applyAdvancement` applies an advancement to advance the character a single step.
--
-- Additional inference should be added to one of these functions.
-- + `prepareCharacter` (see below) if it applies to Character Generation only
-- + `prepareAdvancement` if it modifies the advancement only
-- + `applyAdvancement` if it modifies the CharacterState
instance Advance Character where
   advance ct c | isNothing (state c) = advance ct $ prepare c
                | futureAdvancement c == [] = c
                | ct < ct' = c
                | otherwise =  advance ct $ step c 
            where y =  head $ futureAdvancement c
                  ct' =  season y

   stepIf ns = trace ("stepIf: "++show ns) $ completeAdv . applyAdv . nextAdv ns

   step c = c { state = Just cs
              , pastAdvancement = (a:xs)
              , futureAdvancement = ys
              }
            where (y:ys) = futureAdvancement c
                  xs = pastAdvancement c
                  (a,cs) = applyAdvancement (prepareAdvancement cstate y) cstate
                  cstate = fromJust $ state c

   nextAdvancement = f . futureAdvancement
       where f [] = NoTime
             f (x:_) = season x
   prepare = prepareCharacter

-- | Apply advancement
-- This function is generic, and used for both chargen and ingame 
-- advancement.  The AugmentedAdvancement has to be prepared differently,
-- using either `prepareAdvancement` or `prepareCharGen`.
applyAdvancement :: AugmentedAdvancement
                 -> CharacterState 
                 -> (AugmentedAdvancement,CharacterState)
applyAdvancement a cs = (a,cs')
    where cs' = cs { charTime = season a, traits = new }
          new = advanceTraitList change tmp
          tmp = advanceTraitList inferred old
          change = sortTraits $ changes a
          inferred = sortTraits $ inferredTraits a
          old = sortTraits $ traits cs

applyAdv :: (Character,Maybe AugmentedAdvancement)
         -> (Character,Maybe AugmentedAdvancement)
applyAdv (c,Nothing) = (c,Nothing)
applyAdv (c,Just a) = (c',Just a')
    where (a',st') = applyAdvancement a st
          c' = c { state = Just st' }
          st = trace (show $ characterSeason c ) $ fromMaybe defaultCS $ state c

completeAdv :: (Character,Maybe AugmentedAdvancement)
                 -> Character
completeAdv (c,Nothing) = c 
completeAdv (c,Just a) = c { pastAdvancement = a:pastAdvancement c }

-- |
-- == Char Gen

-- | Compute the initial state if no state is recorded.
-- The function uses `applyCGA` to process all of the pregame advancements.
-- It then calls `addConfidence` to add the confidence trait to the state
-- for the returned `Character` object
prepareCharacter :: Character -> Character
prepareCharacter c 
            | state c /= Nothing = c
            | otherwise = c { state = newstate
                            , pregameDesign = xs
                            , pregameAdvancement = []
                            , entryTime = f $ futureAdvancement c
                            }
            where as = pregameAdvancement  c 
                  (xs,cs) = applyCGA as defaultCS { charSType = charType $ concept c }
                  newstate = Just $ addConfidence $ cs { charTime = GameStart }
                  f [] = NoTime
                  f (x:_) = season x

-- | Augment and amend the advancements based on current virtues and flaws.
--
-- This function is applied by `applyCharGenAdv` before the advancement is
-- applied to the `CharacterState`.  It infers additional traits from 
-- virtues and flaws, add XP limits to the advancements, and checks that
-- the advancement does not overspend XP or exceed other limnits.
prepareCharGen :: CharacterState -> Advancement -> AugmentedAdvancement
prepareCharGen cs = validateCharGen sheet   -- Validate integrity of the advancement
                  . sortInferredTraits      -- Restore sort order on inferred traits
                  . agingYears              -- add years of aging as an inferred trait
                  . initialLimits (filterCS cs)        -- infer additional properties on the advancement
                  . addInference cs         -- infer additional traits 
          where sheet = filterCS cs

-- | Infer an aging trait advancing the age according to the advancement
agingYears :: AugmentedAdvancement -> AugmentedAdvancement
agingYears x | y > 0 = x { inferredTraits = agePT y: inferredTraits x }
             | otherwise = x
   where y = fromMaybe 0 $ augYears x


-- | Add the Confidence trait to the character state, using 
addConfidence :: CharacterState -> CharacterState
addConfidence cs = cs { traits = sortTraits $ ct:traits cs }
          where vfs = vfList sheet
                sheet = filterCS cs
                ct | csType sheet == Grog = ConfidenceTrait $ Confidence
                           { cname = "Confidence", cscore = 0, cpoints = 0 }
                   | otherwise = inferConfidence vfs 


-- | Apply CharGen advancement
applyCharGenAdv :: Advancement -> CharacterState -> (AugmentedAdvancement,CharacterState)
applyCharGenAdv a cs = (a',f cs')
   where (a',cs') = applyAdvancement ( prepareCharGen cs a ) cs
         (PostProcessor g) = postProcessTrait a'
         f x = x { traits = map g $ traits x }

-- | Apply a list of advancements
applyCGA :: [Advancement] -> CharacterState -> ([AugmentedAdvancement],CharacterState)
applyCGA a cs = applyCGA' ([],a,cs)

-- | Recursive helper for `applyCGA`.
applyCGA' :: ([AugmentedAdvancement],[Advancement],CharacterState)
                   -> ([AugmentedAdvancement],CharacterState)
applyCGA' (xs,[],cs) = (xs,cs)
applyCGA' (xs,y:ys,cs) = applyCGA' (a':xs,ys,cs')
    where (a',cs') = applyCharGenAdv y cs


-- |
-- == Preparing the Advancement

-- |
-- Get the next augmented advancement.
nextAdv :: SeasonTime -> Character -> (Character,Maybe AugmentedAdvancement)
nextAdv ns ch | fs == [] = (ch,Nothing)
              | season adv > ns = (ch,Nothing)
              | otherwise = (new,Just a)
        where a = prepareAdvancement (fromJust st) adv
              st = state ch
              (adv:as) = fs
              fs = futureAdvancement ch
              new = ch { futureAdvancement = as }

-- | Augment and amend the advancements based on current virtues and flaws.
prepareAdvancement :: CharacterState -> Advancement -> AugmentedAdvancement
prepareAdvancement c = validate 
                     . sortInferredTraits   -- sort inferred traits
                     . inferSQ c
                     . winterEvents c 
                     . addInference c

-- | Sort the `inferredTraits` field of an `AugmentedAdvancement`.
sortInferredTraits :: AugmentedAdvancement -> AugmentedAdvancement
sortInferredTraits x = x { inferredTraits = sortTraits $ inferredTraits x }

-- | Handle aging and some warping for Winter advancements.
-- Non-winter advancements are left unmodified.
winterEvents :: CharacterState       -- ^ Current Character State
             -> AugmentedAdvancement -- ^ Advancement 
             -> AugmentedAdvancement -- ^ modified Advancement
winterEvents c a | isWinter $ season a  
             = validateAging (y >* yl) agingOb  -- check for aging roll is made if required
             $ addYear agingOb                  -- add a yer of aging
             $ warpingLR a                      -- add warping point for LR
             | otherwise = a
        where ageOb = ageObject c
              y = age c
              pt = find ( (AgeKey ==) . traitKey ) $ changes a
              agingOb | isNothing pt = Nothing
                      | otherwise = aging $ fromJust pt
              lr | ageOb == Nothing = -1
                 | otherwise = longevityRitual $ fromJust ageOb
              yl | ageOb == Nothing = trace "No age object" 35
                 | otherwise = ageLimit $ fromJust ageOb
              warpingLR x | lr < 0 = x
                          | otherwise = x { inferredTraits = 
                                    defaultPT { other = Just "Warping"
                                              , points = Just 1
                                              , comment = Just "from Longevity Ritual" }
                                    :inferredTraits x }
              addYear o x | addsYear o = x
                          | otherwise = x { inferredTraits = agePT 1 :inferredTraits x }
              addsYear Nothing = False
              addsYear (Just x) | isNothing (addYears x) = False
                                | fromJust (addYears  x) <= 0 = False
                                | otherwise = True
              validateAging False _ x =  x
              validateAging True Nothing x = trace ("No aging> "++show a) $ x { validation = err:validation x }
              validateAging True (Just ob) x
                   | isNothing (agingRoll ob) = x { validation = err:validation x }
                   | otherwise =  x { validation = val:validation x }
              err = ValidationError $ "Older than " ++ show yl ++ ". Aging roll required."
              val = Validated $ "Aging roll made"

-- | Return a `ProtoTrait` for aging advancing a number of years.
agePT :: Int -- ^ Number of years
      ->  ProtoTrait -- ^ Resulting ProtoTrait
agePT x = defaultPT { aging = Just $ defaultAging { addYears = Just x } }

-- | Calculate initial XP limits on Advancements
inferSQ :: CharacterState -> AugmentedAdvancement -> AugmentedAdvancement
inferSQ cs ad = ad { baseSQ = sq, bonusSQ = vfBonusSQ vf ad }
        where vf = vfList $ filterCS cs
              (sq,cap) = getSQ ad
-- Infer SQ for Exposure = 2
-- Infer SQ for reading from book
-- Infer SQ for taught from teacher
-- Infer SQ for adventure from covenant

bookSQ :: AugmentedAdvancement -> AugmentedAdvancement 
bookSQ aa | isNothing stats = aa
          | isNothing tr = aa
          | otherwise = aa 
    where tr = ttrace $ primaryXPTrait $ advancement aa
          stats = find ctp $ foldl (++) [] $ map bookStats $ bookUsed aa
          ctp =  (==(fromJust tr)) . topic 


getSQ :: AugmentedAdvancement -> (Maybe XPType,Maybe Int)
getSQ a | isExposure ad = (Just 2,Nothing)
        | mode ad == Reading = rd 
        | otherwise = mstat
   where ad = advancement a
         mstat = (sourceQuality ad,sourceCap ad)
         rd = (fmap fromIntegral $ quality bk,bookLevel bk)
         bk = head $ bookStats $ head $ bookUsed a

-- |
-- Calculate the Source Quality the character generates as a teacher.
charTeacherSQ :: CharacterState -> Int
charTeacherSQ cs = 3 + com + tch
    where sheet = filterCS cs
          com = sheetCharacteristicScore sheet (CharacteristicKey "Com")
          (tch,tspec) = sheetAbilityScore sheet (CharacteristicKey "Teaching")
          -- add good teacher
          -- subtract flaws
          -- add speciality
          -- add one/two student bonus
-- Teacher SQ +

