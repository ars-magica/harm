{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.Markdown
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Classes and instances to make MarkDown output.
--
-- The core of this module is the 'Markdown' class and its 'printMD'
-- function which renders an object in Markdown.  There is also 
-- a 'LongSheet' class with a 'printSheetMD' function for a more
-- verbose character sheet.
--
-----------------------------------------------------------------------------
module ArM.Markdown.Markdown ( Markdown(..)
                    , artMD
                    , artVisMD
                    , italicOString
                    , storyOList
                    , sagaStateMD
                    , enchantedMD  -- Unused
                    ) where

import Data.Maybe 
import Control.Monad

import ArM.Markdown.HOutput 
import ArM.Markdown.Possession 
import ArM.Markdown.Spell
import ArM.Character 
import ArM.Types.Harm
import ArM.Story
import ArM.Sheet
import ArM.Trait
import ArM.DB
import ArM.GameRules
import ArM.Helper

import Data.OList
import Data.HList
import Data.KeyPair

import ArM.Debug.Trace

-- * The Markdown class

-- | Class defining 'printMD' to render in Markdown.
class Markdown a where
     -- | This is the basic function to render in Markdown
     printMD :: a           -- ^ object to render
             -> OList       -- ^ list of lines for output

     -- | This is a hack to augment characters using extra resources
     -- By default, it is identical to 'printMD'.
     printSheetMD :: Saga      -- ^ Saga including databases for spells etc.
                -> a         -- ^ object to render
                -> OList     -- ^ list of lines for output
     printSheetMD _ = printMD

-- ** Top level entities
--
-- $markdown
-- Top-level functions are entitites with their own pages:
-- + Saga top level page $\to$ `printSheetMD`
-- + Saga state (per season) $\to$ `sagaStateMD`
-- + Character $\to$ `printSheetMD`
-- + Covenatn $\to$ `printSheetMD`
--

-- | Render the state page for the Saga
sagaStateMD :: Saga -> OList 
sagaStateMD = fromHList . sagaStateH


instance Markdown Saga where
    printMD = defaultMD

instance Markdown Character where
   printMD _ = error "Character printMD is not supported"
   printSheetMD saga c = OList
            [ printMD $ concept c
            , OString ""
            , OString $ "## Character Sheet " ++ (show $ gameSeason c) 
            , OString ""
            , sheetSheetMD saga c
            -- , adv
            , designMD c
            , advancementMD c
            ]
        where adv | isGameStart c = designMD c
                  | otherwise =  advancementMD c
instance Markdown Covenant where
    printMD = defaultMD
    printSheetMD = defaultSheetMD

-- ** Lower-level concepts

instance Markdown CharacterConcept where
   printMD = defaultMD
   printSheetMD = defaultSheetMD

instance Markdown Trait where
   printMD = defaultMD
instance Markdown ProtoTrait where
   printMD = defaultMD


-- ** Markdown for basic types

instance Markdown a => Markdown (Maybe a) where
   printMD Nothing = OList []
   printMD (Just x) = printMD x
   printSheetMD _ Nothing = OList []
   printSheetMD saga (Just x) = printSheetMD saga x

instance Markdown KeyPairList where
   printMD = defaultMD

-- * Other Functions

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
--
-- This is usually empty, since pre-game characters are not usuall produced.
designMD :: Character -> OList
designMD = fromMaybe (OList []) . fmap fromHList . designH


-- | Render the advancement log.
-- This is two lists of past and future advancement objects
advancementMD :: Character -> OList
advancementMD = fromHList . advancementH


-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of 'Show'.
showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]
 
-- * Markdown for the Character types
 
instance Markdown MagicEffect  where
   printMD = defaultMD 
instance Markdown Enchantment  where
   printMD (LesserItem eff) = printMD eff 
   printMD (GreaterDevice vn eff) = OList 
       [ OString $ "Greater Enchanted Device (opened with " ++ show vn ++ "p vis)"
       , OList $ map printMD eff ]
   printMD (Talisman vn eff) = OList 
       [ OString $ "Talisman (opened with " ++ show vn ++ "p vis)"
       , OList $ map printMD eff ]
   printMD (ChargedItem vn eff) = OList 
       [ OString $ "Charged Item (" ++ show vn ++ "charges)"
       , OList [ printMD eff ] ]
   printMD MundaneItem = OString "Mundane Item" 

enchantedMD :: Possession -> Enchantment -> OList
enchantedMD _ MundaneItem = OList []
enchantedMD ob (LesserItem eff) = OList 
         [ OString $ pName ob ++ tf ++ "lesser enchanted device"
                   , f $ printMD eff ]
   where f (OList xs) = foldOList $ OList $ mtail xs
         f os = os
         tf = " (" ++ teforql eff ++ ") "

enchantedMD ob enc = OList [ OString $ pName ob 
                          , printMD $ enc
                           ]

instance Markdown Possession  where
   printMD = defaultMD

instance Markdown Library where
   printMD = defaultMD

sheetSheetMD :: Saga -> Character -> OList
sheetSheetMD saga c = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ sortTraits $ charList c
               , showlistMD "+ **Personality Traits:** "  $ sortTraits $ ptList c
               , showlistMD "+ **Reputations:** "  $ sortTraits $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( sortTraits $ abilityList c )) ]
               , fromHList $ listPossessionsH $ characterPossessions c
               , OString ""
               , printCombatMD saga c
               , mag
               ]
         where spellist = spellsWithScores (spells saga) c 
               mag | isMagus c = OList 
                       [ artVisMD c
                       , OString ""
                       , printFullGrimoire (spells saga) $ sortTraits spellist
                       , OString ""
                       , toOList $ printCastingTotals c 
                       , OString ""
                       , OString $ "+ Ceremonial Casting Bonus: " ++ showSigned (ceremonialCastingBonus c)
                       , OString ""
                       , OString "## Laboratory"
                       , OString ""
                       , toOList $ printLabTotals c 
                       , OString ""
                       , printSheetMD saga $ characterLab c
                       , OString ""
                       ]
                   | otherwise = OString "" 


-- ** Markdown for Age, Confidence, Warping, and Decrepitude

-- | Print age, confidence, warping, and decrepitude as bullet points
briefTraits :: Character -> OList
briefTraits = fromMaybe (OList []) . fmap fromHList . briefTraitsH

instance Markdown Age where
   printMD = defaultMD

-- | Print a table of casting totals for every TeFo combination.
printCastingTotals :: Character -> [String]
printCastingTotals c 
             | Magus /= characterType c = []
             | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (castingTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

-- | Print a table of casting totals for every TeFo combination.
printLabTotals :: Character -> [String]
printLabTotals c 
             | Magus /= characterType c = []
             | otherwise = "":"| Lab Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

instance Markdown Confidence where
   printMD = defaultMD
instance Markdown OtherTrait where
   printMD = defaultMD

-- * Advancements

instance (Markdown a, ContractAdvancement a) 
      => Markdown (Augmented a) where
   printMD = printMD . contractAdvancement
instance Markdown CovAdvancement where
   printMD ad = OList $ sls ++ f ch
      where ch = printCovChanges ad
            sls = foldl (++) [] ( map ( f . printMD ) $ caStory ad )
            f (OList x) = x
            f (OString "") = []
            f (OString xs) = [OString xs]
instance Markdown Story where
   printMD story = OList 
         [ OString $ storyTitle story ++ sq (storySQ story) 
         , OList $ map italicOString ( storyNarrative story )
         , OList $ map OString ( storyComment story )
         ]
      where sq Nothing = "(no source quality)"
            sq (Just x) = " (SQ " ++ show x ++ ")"
printCovChanges :: CovAdvancement -> OList
printCovChanges a = OList [ OString "Changes", OList [ j, lv, acq, lst ] ]
     where j | joining a == [] = OList []
             | otherwise = OString $  "joining: " ++ showStrList (map show $ joining a)
           lv | leaving a == [] = OList []
             | otherwise = OString $  "leaving: " ++ showStrList (map show $ leaving a)
           acq | acquired a == [] = OList []
             | otherwise = OString $  "acquired: " ++ showStrList (map name $ acquired a)
           lst | lost a == [] = OList []
             | otherwise = OString $  "lost: " ++ showStrList (map name $ lost a)

instance Markdown Advancement where
   printMD = defaultMD


-- ** Pretty print arts

-- | Render art scores as a table
artMD :: Character
      -> OList
artMD c | isMagus c = toOList $ artMD' c
        | otherwise = OList []

-- | Render art scores as a table
artMD' :: Character
      -> [ String ]
artMD' = ("":) . (h1:) . (h2:) . map artLine . sortTraits . artList 
   where h1 = "| Art  | Score | XP |" 
         h2 = "| -: | -: | -: |"


-- | Auxiliary for 'artMD', rendering a single line in the table
artLine :: Art -> String
artLine ar = "| " ++ artName ar  ++ " | " ++ show (artScore ar) ++ " | " ++ showNum (artExcessXP ar) ++ " |"

-- | Render art scores and vis stocks as a table
artVisMD :: Character
      -> OList
artVisMD c | isMagus c = toOList $ artVisMD' c
        | otherwise = OList []

-- | Render art scores and vis stocks as a table
artVisMD' :: Character
          -> [ String ]
artVisMD' = ("":) . (h1:) . (h2:) . artVisBody
   where h1 = "| Art  | Score | XP | Vis |" 
         h2 = "| -: | -: | -: | -: |"

-- | Auxiliar for 'artVisMD'' rendering the body of the table.
artVisBody :: Character
           -> [ String ]
artVisBody cs = map artVisLine $ mergeArt as bs
   where as = (map tupleArt . sortTraits . artList ) cs
         bs = sheetVis cs
         tupleArt a = (traitKey a,artName a, artScore a,artExcessXP a)

-- | Merge lists of art traits and vis possessions.
-- This is rather crude to say the least.
mergeArt :: [(TraitKey,String,Int,XPType)] -> [(TraitKey,Int)] 
         -> [(TraitKey,String,Int,XPType,Int)]
mergeArt [] [] = []
mergeArt [] ((y1,y2):ys) = (y1,f y1,0,0,y2):mergeArt [] ys
     where f (ArtKey xn) = xn
           f _ = trace "ERROR: Not an art in mergeArt." ""
mergeArt ((x1,x2,x3,x4):xs) []  = (x1,x2,x3,x4,0):mergeArt xs [] 
mergeArt ((x1,x2,x3,x4):xs) ((y1,y2):ys) 
     | x1 == y1 = (x1,x2,x3,x4,y2):mergeArt xs ys
     | x1 < y1 = (x1,x2,x3,x4,0):mergeArt xs ((y1,y2):ys) 
     | otherwise = (y1,f y1,0,0,y2):mergeArt ((x1,x2,x3,x4):xs) ys
     where f (ArtKey xn) = xn
           f _ = trace "ERROR: Not an art in mergeArt." ""


-- | Auxiliary for 'artVisMD', rendering a single line in the table
artVisLine :: (TraitKey,String,Int,XPType,Int) -> String
artVisLine (_,s,i1,i2,i3) = 
        "| " ++ s  ++ " | " ++ show i1 ++ " | " ++ showNum i2 ++ " | " ++ show i3 ++ " |"


-- | Set a list of spells.
-- Each spell is set using 'spellMD', and the result is indented as a
-- hierarchical list.
printFullGrimoire :: SpellDB -> [Spell] -> OList
printFullGrimoire db xs = OList [ OString "## Grimoire"
                         , OString ""
                         , OList $ map (indentOList . spellDescMD) ys 
                         , OString ""
                         , OString $ "Total: " ++show (totalLevels xs)  
                            ++ " levels of spells."
                         ]
   where ys = [ (x,f x) | x <- xs ]
         f x = spellTRecord x `mplus` spellLookup (traitKey x) db 


-- | Return the sum of levels in the list of spells.
totalLevels :: [Spell] -> Int
totalLevels = sum . map spellLevel


-- | Set the Combat Stats of the Character as an 'OList'
printCombatMD :: Saga -> Character -> OList
printCombatMD saga cs = fromHList $ printCombatH saga cs

-- * Covenant Markdown


instance Markdown Book where
    printMD = defaultMD

instance Markdown Lab where
   printMD lb = indentOList $ OList 
       [ OString $ name lb
       , OList 
         [ OString $ "Refinement: " ++ showSigned (labRefinement $ labState lb)
         , OString $ "Size: " ++ showSigned (labSize $ labState lb)
         , OString $ "Used size: " ++ used ++ " out of " ++ lim
         , OString $ "Safety: " ++ saf ++ " (" ++ bas ++ sfl ++ ")"
         , OString $ "Aura: " ++ show (labAura $ labState lb)
         , OString $ "Traits: " ++ commaList ts
         , OString $ "Art Specialisations: " ++ commaList arsp
         , OString $ "Activity Specialisations: " ++ commaList acsp
         , OString "Description"
         , OList $ map italicOString $ narrative lb
         , OList $ map OString $ comment lb
         , OString "Virtues and Flaws"
         , foldOList $ OList $ map printMD $ labVirtues $ labState lb
         ]
       ]
       where ts = filter ( (=="") . labSpecialisation ) tb
             arsp = filter ( (=="Art") . labTrait ) tb
             acsp = filter ( (=="Activity") . labTrait ) tb
             tb = totalBonus lb
             used = showSigned $ usedSize lb
             lim = showSigned $ labVirtueLimit lb
             saf = showSigned $ labSafety lb 
             bas = showSigned $ baseSafety lb 
             sfl = showSigned $ safety lb
instance Markdown LabVirtue where
   printMD v = OList [ OString $ name v
                   , OList $ map italicOString $ narrative v
                   , OList $ map OString $ comment v
                   , OList [ OString ts ]
                   ]
        where ts = "Bonuses: " ++ commaList (labVirtueBonus v)
instance Markdown LabBonus where
   printMD = defaultMD

-- * Convenience Functions

storyOList :: StoryObject a => a -> [ OList ]
storyOList ob = 
       [ OString $ name ob
       , OList  $ map italicOString $ narrative ob
       , OList  $ map OString $ comment ob 
       ]
