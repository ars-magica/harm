{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Classes and instances to make MarkDown output.
--
-- The core of this module is the `Markdown` class and its `printMD`
-- function which renders an object in Markdown.  There is also 
-- a `LongSheet` class with a `printSheetMD` function for a more
-- verbose character sheet.
--
-----------------------------------------------------------------------------
module ArM.Markdown ( Markdown(..)
                    , LongSheet(..)
                    , gameStartSheet
                    , currentSheet
                    , artMD
                    , artVisMD
                    ) where

import Data.Maybe 

import ArM.Char.Character 
import ArM.Char.CharacterSheet
import ArM.Char.Trait
import ArM.Char.Combat
import ArM.Cov.Saga
import ArM.Cov.Covenant
import ArM.DB.Spell
import ArM.GameRules
import ArM.BasicIO
import ArM.Helper

-- import ArM.Debug.Trace

-- |
-- = Rendering the Character Sheet

-- | Render a character sheet without advancement log
baseSheet :: Saga -> Character -> OList
baseSheet saga c | isNothing (state c ) = s1
               | otherwise = OList [ s1, s2, OString "", s3 ]
       where 
            s1 = printMDaug saga $ concept  c 
            s2 = printMDaug saga $ state  c 
            s3 = printCombatMD saga $ characterSheet c

-- | Render a character sheet at game start.
-- Unlike the regular `printMD`, this includes only the character design
-- and not ingame advancements.
gameStartSheet :: Saga -> Character -> OList
gameStartSheet saga c = OList
            [ baseSheet saga c
            , OString ""
            , designMD c
            ]

-- | Render the current character sheet without pregame design details.
currentSheet :: Saga -> Character -> OList
currentSheet saga c = OList [ baseSheet saga c, advancementMD c ]

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
designMD :: Character -> OList
designMD c  | as == [] = OList []
            | otherwise = OList
            [ OString "## Game start design"
            , OString ""
            , OList $ map printMD as
            , OString ""
            ]
            where as = pregameDesign c

-- | Render the CharGen advancements, both the processed and unprocessed ones.
chargenMD :: Character -> OList
chargenMD c = OList [ chargenMD' c, designMD c ]

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
chargenMD' :: Character -> OList
chargenMD' c | as == [] = OList []
            | otherwise = OList
              [ OString "## Char Gen Advancements"
              , OString ""
              , OList $ map printMD as
              , OString ""
              ]
            where as = pregameAdvancement c

-- | Render the advancement log.
-- This is two lists of past and future advancement objects
advancementMD :: Character -> OList
advancementMD c = OList [ ao, bo ]
   where as = pastAdvancement c
         bs = futureAdvancement c
         ao | as == [] = OList []
            | otherwise = OList
                [ OString "## Past Advancement"
                , OString ""
                , OList $ map printMD as
                , OString ""
                ]
         bo | bs == [] = OList []
            | otherwise = OList
                [ OString "## Future Advancement"
                , OString ""
                , OList $ map printMD bs
                , OString ""
                ]


-- |
-- = The Markdown Class

-- | Class defining `printMD` to render in Markdown.
class Markdown a where

     -- | This is the basic function to render in Markdown
     printMD :: a           -- ^ object to render
             -> OList       -- ^ list of lines for output

     -- | This is a hack to augment characters using extra resources
     -- By default, it is identical to `printMD`.
     printMDaug :: Saga      -- ^ Saga including databases for spells etc.
                -> a         -- ^ object to render
                -> OList     -- ^ list of lines for output
     printMDaug _ = printMD

instance Markdown a => Markdown (Maybe a) where
   printMD Nothing = OList []
   printMD (Just x) = printMD x
   printMDaug _ Nothing = OList []
   printMDaug saga (Just x) = printMDaug saga x

instance Markdown FieldValue where
   printMD  = OString . show

instance Markdown KeyPair where
   printMD (KeyPair x  y) = OList
         [ OString x
         , OString $ ':':' ':show y
         , OString "" ]
instance Markdown KeyPairList where
   printMD (KeyPairList xs) = OList $ map printMD xs
instance Markdown Trait where
   printMD (AgeTrait x) = printMD  x
   printMD x = OString $ show  x
instance Markdown ProtoTrait where
   printMD = OString . show 

-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of `Show`.
showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]

-- | Render a Maybe String as an OList.
-- Nothing becomes an empty OList and a Just object becomes a single line.
-- Note that this is different from the generic instance for Maybe, because
-- of the difficulties making an instance for String.
stringMD :: Maybe String -> OList
stringMD Nothing = OList []
stringMD (Just x) = OString x

-- |
-- = Markdown for the Character types
-- 
-- The `CharacterConcept` is set as a description list.
-- 
-- This may cause problems with long text values.  It would be worth distinguishing
-- between more fields and use a differfent formatting where long text is expected.

instance Markdown Character where
   printMD  c = OList
            [ bs 
            , designMD c
            , chargenMD c
            , advancementMD c
            ]
       where 
            bs | isNothing (state c ) = s1
               | otherwise = OList [ s1, s2 ]
            s1 = printMD $ concept  c 
            s2 = printMD $ state  c 
   printMDaug saga c = OList
            [ baseSheet saga c
            , designMD c
            , chargenMD c
            , advancementMD c
            ]


instance Markdown CharacterConcept where
   printMD = conceptPrintMD "../images/"
   printMDaug saga = conceptPrintMD dir
      where dir = fromMaybe "../images/" (baseURL saga)

conceptPrintMD :: String -> CharacterConcept -> OList
conceptPrintMD dir c = OList
               [ OString ("# " ++ nm )
               , OString ""
               , img
               , OString $ show (charType c)
               , OString $ ": " ++ ( fromMaybe "-" $ briefConcept c )
               , OString ""
               , OString "Quirk"
               , OString $ ": " ++ ( fromMaybe "-" $ quirk c )
               , OString ""
               , OString "Appearance" 
               , OString $ ": " ++ ( fromMaybe "-" $ appearance c )
               , OString ""
               , OString "Born" 
               , OString brn
               , OString ""
               , OString "Player" 
               , OString $ ": " ++ ( fromMaybe "-" $ player c )
               , OString ""
               , ( printMD $ charGlance c ) 
               , ( printMD $ charData c )
               ]
          where brn | born c == Nothing = ": ??" 
                    | otherwise = ": " ++ (show $ fromJust $ born c)
                img | isNothing (portrait c) = OList []
                    | otherwise = OList [ OString imgfn, OString "" ]
                imgfn = ("![" ++ nm ++ "](" ++ dir ++ fromJust (portrait c) ++ ")")
                nm = fullConceptName c


instance Markdown CharacterSheet where
   printMD c = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ sortTraits $ charList c
               , showlistMD "+ **Personality Traits:** "  $ sortTraits $ ptList c
               , showlistMD "+ **Reputations:** "  $ sortTraits $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
               , showlistMD "+ **Abilities:** "  $ sortTraits $ abilityList c
               , showlistMD "+ **Arts:** "  $ sortTraits $ artList c
               , showlistMD "+ **Spells:** "  $ sortTraits $ spellList c
               , showlistMD "+ **Possessions:** "  $ sortTraits $ possessionList c
               , toOList $ printCastingTotals c
               ]
   printMDaug saga = printMD . addCastingScores (spells saga)

instance Markdown CharacterState where
   printMD c = OList
       [ OString $ "## " ++ (show $ charTime c )
       , OString ""
       , printMD $ filterCS c
       ]
   printMDaug saga c = OList
       [ OString $ "## " ++ (show $ charTime c) 
       , OString ""
       , printMDaug saga $ filterCS c
       ]

-- |
-- == Markdown for Age, Confidence, Warping, and Decrepitude

-- | Print age, confidence, warping, and decrepitude as bullet points
briefTraits :: CharacterSheet -> OList
briefTraits c = OList
          [ printMD (ageObject c)
          , OList $ map printMD $ confList c
          , OList $ map printMD $ otherList c
          ]

-- | Print a table of casting totals for every TeFo combination.
printCastingTotals :: CharacterSheet -> [String]
printCastingTotals c 
             | Magus /= csType c = []
             | otherwise = "":"| Casting Total | Creo | Intellego | Muto | Perdo | Rego |":
                              "|         :-    |  -:  |  -:       |  -:  |  -:   |  -:  |":
                              lts
      where
          lts = [ "| " ++ fo ++ foldl (++) "" (map ( (" | "++) . show ) ts ) ++ " |" 
                | (fo,ts) <- zip lforms (labTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

instance Markdown Age where
   printMD c = OString $ "+ **Age:** " ++ show y ++ " years (apparent age " 
            ++ show (y - apparentYounger c)  ++ ")" ++ lr
      where y = ageYears c
            lrs = longevityRitual c
            lr | lrs < 0 = ""
               | otherwise = " Longevity Ritual: " ++ show lrs
instance Markdown Confidence where
   printMD c = OString $
             "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" 
instance Markdown OtherTrait where
   printMD c = OString $
             "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" 



-- |
-- = Advancements

-- | Render the source quality of an advancement
showSQ :: Maybe XPType -> Maybe XPType -> String
showSQ Nothing Nothing = ""
showSQ (Just x) Nothing = " (" ++ show x ++ "xp)"
showSQ Nothing (Just x) = " (" ++ show x ++ "xp)"
showSQ (Just x) (Just y) = " (" ++ show x ++ f (y-x) ++ "xp)"
    where f 0 = ""
          f z = "+" ++ show z

instance Markdown AugmentedAdvancement where
   printMD a = indentOList $ OList
       [ OString $ showTime xps (season a) (mode a) y 
       , OList [ stringMD $ narrative a ]
       , OList $ map printMD $ changes a
       , infl
       , OList $ map (OString . show) $ validation a
       ]
      where xps = showSQ (sourceQuality a) (effectiveSQ a)
            y = augYears a
            inf = inferredTraits a
            infl | inf == [] = OList []
                 | otherwise = OList [ OString "Inferred traits", OList $ map printMD inf ]


-- | Render the season and mode of an advancement
showTime :: String -> SeasonTime -> Maybe String -> Maybe Int -> String
showTime xps NoTime Nothing y = ("?? " ++ xps ++ showYears y)
showTime xps  x Nothing y = (show x ++ xps ++ showYears y)
showTime xps NoTime (Just x) y = (x ++ xps ++ showYears y)
showTime xps x (Just z) y = (show x ++ xps ++ showYears y ++ " " ++ z)

-- | Render the duration of an advancement
showYears :: Maybe Int -> String
showYears Nothing = ""
showYears (Just x) = " (" ++ show x ++ " years)"

instance Markdown Advancement where
   printMD a = indentOList $ OList
         [ OString $ showTime xps (season a) (mode a) y 
         , OList [ stringMD $ narrative a ]
         , OList $ map printMD $ changes a
         ]
      where xps | sx == Nothing = ""
                | otherwise = " (" ++ ishow sx ++ "xp)" 
            sx = sourceQuality a
            ishow = show . fromJust
            y = advYears a

-- |
-- = Long Sheet Format


-- | The `LongSheet` class is similar to `Markdown`, but is
-- intended to make a longer output with both more space and more
-- verbose text.  
--
-- In the current impplementation, it is only the character state
-- which is made longer, by setting abilities and spells as bullet
-- points instead of a single paragraph for the full list.
class Markdown a => LongSheet a where
   -- | By default `printSheetMD` is identical to `printMDaug`
   printSheetMD :: Saga    -- ^ Saga including databases for spells, etc.
                -> a       -- ^ object to render
                -> OList   -- ^ list of lines for output
   printSheetMD = printMDaug

instance LongSheet Character where
   printSheetMD saga c = OList 
            [ printMD $ concept c
            , sf $ state c 
            , adv
            ]
        where sf Nothing = OList []
              sf (Just s) = printSheetMD saga s
              adv | isGameStart c = chargenMD c
                  | otherwise =  advancementMD c
instance LongSheet CharacterState where
   printSheetMD saga c = OList [ OString $ "## " ++ (show $ charTime c )
                             , OString ""
                             , printSheetMD saga $ filterCS c ]

instance LongSheet CharacterSheet where
   printSheetMD saga c' = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ sortTraits $ charList c
               , showlistMD "+ **Personality Traits:** "  $ sortTraits $ ptList c
               , showlistMD "+ **Reputations:** "  $ sortTraits $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( sortTraits $ abilityList c )) ]
               , indentOList $ OList $ [ OString "**Possessions:**"
                        , OList (map (OString . show) ( sortTraits $ possessionList c )) ]
               , OString ""
               , printCombatMD saga c
               , mag
               ]
         where c = addCastingScores (spells saga) c'
               mag | isMagus c' = OList [ artVisMD c
                                        , OString ""
                                        , printFullGrimoire (spells saga) $ sortTraits $ spellList c 
                                        , OString ""
                                        , toOList $ printCastingTotals c 
                                        , OString ""
                                        ]
                   | otherwise = OString "" 

-- |
-- == Pretty print arts

-- | Render art scores as a table
artMD :: CharacterSheet
      -> OList
artMD c | isMagus c = toOList $ artMD' c
        | otherwise = OList []

-- | Render art scores as a table
artMD' :: CharacterSheet
      -> [ String ]
artMD' = ("":) . (h1:) . (h2:) . map artLine . sortTraits . artList 
   where h1 = "| Art  | Score | XP |" 
         h2 = "| -: | -: | -: |"


-- | Auxiliary for `artMD`, rendering a single line in the table
artLine :: Art -> String
artLine ar = "| " ++ artName ar  ++ " | " ++ show (artScore ar) ++ " | " ++ showNum (artExcessXP ar) ++ " |"

-- | Render art scores and vis stocks as a table
artVisMD :: CharacterSheet
      -> OList
artVisMD c | isMagus c = toOList $ artVisMD' c
        | otherwise = OList []

-- | Render art scores and vis stocks as a table
artVisMD' :: CharacterSheet
          -> [ String ]
artVisMD' = ("":) . (h1:) . (h2:) . artVisBody
   where h1 = "| Art  | Score | XP | Vis |" 
         h2 = "| -: | -: | -: | -: |"

-- | Auxiliar for `artVisMD'` rendering the body of the table.
artVisBody :: CharacterSheet
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
mergeArt [] ((y1,y2):ys) = (y1,xn,0,0,y2):mergeArt [] ys
     where (ArtKey xn) = y1
mergeArt ((x1,x2,x3,x4):xs) []  = (x1,x2,x3,x4,0):mergeArt xs [] 
mergeArt ((x1,x2,x3,x4):xs) ((y1,y2):ys) 
     | x1 == y1 = (x1,x2,x3,x4,y2):mergeArt xs ys
     | x1 < y1 = (x1,x2,x3,x4,0):mergeArt xs ((y1,y2):ys) 
     | otherwise = (y1,xn,0,0,y2):mergeArt ((x1,x2,x3,x4):xs) ys
     where (ArtKey xn) = y1


-- | Auxiliary for `artVisMD`, rendering a single line in the table
artVisLine :: (TraitKey,String,Int,XPType,Int) -> String
artVisLine (_,s,i1,i2,i3) = 
        "| " ++ s  ++ " | " ++ show i1 ++ " | " ++ showNum i2 ++ " | " ++ show i3 ++ " |"


-- |
-- == Render Spells

-- | Render a spell trait in Markdown
-- The result should normally be subject to indentOList to make an hierarchical
-- list.
spellDescMD :: (Spell,Maybe SpellRecord) -> OList
spellDescMD (s,sr) = OList [ OString $ show s
                  , OList [ masteryMD s, f $ spellTComment s ]
                  , coreSpellRecordMD sr
                  ]
     where f "" = OList [] 
           f x = OString x

-- | Set all information from mastery on one line.
-- This includes mastery score, xp, and mastery options.
masteryMD :: Spell -> OList
masteryMD s | 0 == masteryScore s && 0 == spellExcessXP s = OList []
            | otherwise = OString
                          $ "Mastery: " ++ show (masteryScore s)
                          ++ " (" ++ showNum (spellExcessXP s) ++ "xp) "
                          ++ showStrList (masteryOptions s)


-- | Set a list of spells.
-- Each spell is set using `spellMD`, and the result is indented as a
-- hierarchical list.
printFullGrimoire :: SpellDB -> [Spell] -> OList
printFullGrimoire db xs = OList [ OString "## Grimoire"
                         , OString ""
                         , OList $ map (indentOList . spellDescMD) ys 
                         , OString ""
                         , OString $ "Total: " ++show (totalLevels xs)  
                            ++ " levels of spells."
                         ]
   where ys = [ (x,spellLookup (traitKey x) db ) | x <- xs ]


-- | Return the sum of levels in the list of spells.
totalLevels :: [Spell] -> Int
totalLevels = sum . map spellLevel

-- | Render the spell record as an OList
coreSpellRecordMD :: Maybe SpellRecord -> OList
coreSpellRecordMD Nothing = OList []
coreSpellRecordMD sr = OList [ reqstr
                             , OString $ (showRDT sp) ++ spstr
                             , os (description sp)
                             , os (design sp)
                             , os (cite sp)
                             ]
   where req = techniqueReq sp ++ formReq sp
         sp = fromJust sr
         os "" = OList []
         os x = OString x
         reqstr | req == [] = OList []
                | otherwise = OString $ "Req. " ++ showStrList req
         spstr | [] == specialSpell sp = ""
               | otherwise = "; " ++ showStrList (specialSpell sp)

showRDT :: SpellRecord -> String
showRDT sp = "Range: " ++ r ++
             "; Duration: " ++ d ++
             "; Target: " ++ t
   where (r,d,t) = rdt sp

-- | Set the Combat Stats of the Character as an `OList`
printCombatMD :: Saga -> CharacterSheet -> OList
printCombatMD saga cs = OList x
    where tab = computeCombatStats ( weapons saga ) cs
          x | tab == [] = []
            | otherwise = [ combatHead, combatBody tab ]

-- | Set the table body for `printCombatMD`
combatBody :: [CombatLine] -> OList
combatBody = OList . map combatBodyLine

-- | Set a single line for `printCombatMD`
combatBodyLine :: CombatLine -> OList
combatBodyLine c = OString $ "| " ++ (combatLabel c) ++ 
                            " | " ++ (show $ combatInit c) ++
                            " | " ++ (showstat $ combatAtk c) ++
                            " | " ++ (showstat $ combatDef c) ++
                            " | " ++ (showstat $ combatDam c) ++
                            " | " ++ (showstat $ combatRange c) ++
                            " | " ++ (show $ combatLoad c) ++
                            " | " ++ (combatComment c) ++
                            " |"

-- | Set the header for `printCombatMD`
combatHead :: OList
combatHead = OList [ OString "| Weapon | Init | Atk | Def | Dam | Range | Load | Comment |"
                   , OString "|  :- |  -: |  -: |  -: |  -: |  -: |  -: | :- |"
                   ]


-- |
-- = Saga Markdown

instance Markdown Saga where
    printMD saga = OList 
        [ OList [ OString $ "# " ++ sagaTitle saga
                , OString ""
                , OString $ sagaDesc saga
                , OString ""
                ]
        , OList $ [ OString $ "+ " ++ pagesLink (show $ seasonTime st) | st <- sagaStates saga ]
        , OString "" 
        , OString "+ [JSON Syntax Errors](syntaxcheck.txt) in the character files"
        ]

instance Markdown SagaState where
    printMD saga = OList 
        [ OString $ "# " ++ sagaStateName saga 
        , OString ""
        , characterIndex $ characters saga
        , OString ""
        , covenantIndex $ covenants saga
        , OString ""
        , OString "## Advancement Errors"
        , OString ""
        , indentOList $ foldOList $  advancementErrors saga
        ]

-- |
-- = Covenant Markdown

instance LongSheet Covenant 
instance Markdown Covenant where
    printMD cov = OList 
        [ OString $ "# " ++ (covName $ covenantConcept cov )
        , OString ""
        , printMD $ covenantConcept cov
        , OString ""
        , printMD $ covenantState cov
        ]

instance LongSheet CovenantConcept
instance Markdown CovenantConcept where
    printMD cov = OList  
       [ OString $ fromMaybe "" $ covConcept cov
       , OString ""
       , OString $ "*Founded* " ++ (show $ covFounded cov)
       , OString ""
       , OString app
       ]
       where app | isNothing (covAppearance cov) = ""
                 | otherwise = "**Appearance** " ++ (fromJust $ covAppearance cov)

instance LongSheet CovenantState
instance Markdown CovenantState where
    printMD cov = OList  
        [ OString $ "## " ++ (show $ covTime cov)
        ]

