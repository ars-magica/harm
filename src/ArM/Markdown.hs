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
                    , artMD
                    , artVisMD
                    , stringMD
                    , formatTitle
                    ) where

import Data.Maybe 

import ArM.Char.Character 
import ArM.Types.ProtoTrait
import ArM.Char.Combat
import ArM.Cov.Saga
import ArM.Types.Covenant
import ArM.Types.Library
import ArM.Types.Saga
import ArM.Types
import ArM.DB.Spell
import ArM.GameRules
import ArM.BasicIO
import ArM.Helper

-- import ArM.Debug.Trace

-- |
-- = Rendering the Character Sheet


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
     printSheetMD :: Saga      -- ^ Saga including databases for spells etc.
                -> a         -- ^ object to render
                -> OList     -- ^ list of lines for output
     printSheetMD _ = printMD

instance Markdown a => Markdown (Maybe a) where
   printMD Nothing = OList []
   printMD (Just x) = printMD x
   printSheetMD _ Nothing = OList []
   printSheetMD saga (Just x) = printSheetMD saga x

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
   printSheetMD saga c = OList
            [ printMD $ concept c
            , sf $ state c 
            , adv
            ]
        where sf Nothing = OList []
              sf (Just s) = printSheetMD saga s
              adv | isGameStart c = chargenMD c
                  | otherwise =  advancementMD c


instance Markdown CharacterConcept where
   printMD = conceptPrintMD "../images/"
   printSheetMD saga = conceptPrintMD dir
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

pList :: [ Possession ] -> OList
pList = OList  . map (OString . show ) . sortTraits 

{-
listPossessions :: [ Trait ] -> OList
listPossessions = listPossessions' . filterNothing . map f
   where f (PossessionTrait p) = Just p
         f _ =  Nothing
-}
listPossessions :: [ Possession ] -> OList
listPossessions ps = OList
      [ OString "Vis"
      , (pList vs)
      , OString "Weapons"
      , (pList ws)
      , OString "Armour"
      , (pList as)
      , OString "ArcaneConnections"
      , (pList acs)
      , OString "Equipment"
      , (pList es)
      ]
   where vs = filter isVis ps
         ws = filter isWeapon ps
         as = filter isArmour ps
         acs = filter isAC ps
         es = filter isEquipment ps

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
               , OString ""
               , OString "## Laboratory"
               , OString ""
               , toOList $ printLabTotals c
               ]
   printSheetMD saga c' = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ sortTraits $ charList c
               , showlistMD "+ **Personality Traits:** "  $ sortTraits $ ptList c
               , showlistMD "+ **Reputations:** "  $ sortTraits $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( sortTraits $ abilityList c )) ]
               , indentOList $ listPossessions $ possessionList c
               -- , indentOList $ OList $ [ OString "**Possessions:**"
                        -- , OList (map (OString . show) ( sortTraits $ possessionList c )) ]
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
                                        , OString "## Laboratory"
                                        , OString ""
                                        , toOList $ printLabTotals c 
                                        , OString ""
                                        , printSheetMD saga $ characterLab c
                                        , OString ""
                                        ]
                   | otherwise = OString "" 

instance Markdown CharacterState where
   printMD c = OList
       [ OString $ "## " ++ (show $ charTime c )
       , OString ""
       , printMD $ characterSheet c
       ]
   printSheetMD saga c = OList
       [ OString $ "## " ++ (show $ charTime c) 
       , OString ""
       , printSheetMD saga $ characterSheet c
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
                | (fo,ts) <- zip lforms (castingTotals c) ]
          lforms = [ "Animal", "Aquam", "Auram", "Corpus", "Herbam", "Ignem", "Imaginem", "Mentem", "Terram", "Vim" ]

-- | Print a table of casting totals for every TeFo combination.
printLabTotals :: CharacterSheet -> [String]
printLabTotals c 
             | Magus /= csType c = []
             | otherwise = "":"| Lab Total | Creo | Intellego | Muto | Perdo | Rego |":
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


instance Markdown AugCovAdvancement where
   printMD = printMD . contractAdvancement
instance Markdown CovAdvancement where
   printMD ad = foldOList $ OList $ ( map printMD $ caStory ad ) ++ ch
      where (OList ch) = printCovChanges ad
instance Markdown Story where
   printMD story = OList 
         [ OString $ storyTitle story ++ sq (storySQ story) 
         , OList $ map OString ( storyNarrative story )
         , OList $ map OString ( storyComment story )
         ]
      where sq Nothing = "(no source quality)"
            sq (Just x) = " (SQ " ++ show x ++ ")"
printCovChanges :: CovAdvancement -> OList
printCovChanges a = OList [ OString "Changes", j, lv, acq, lst ]
     where j | joining a == [] = OList []
             | otherwise = OString $  "    + joining: " ++ showStrList (map show $ joining a)
           lv | leaving a == [] = OList []
             | otherwise = OString $  "    + leaving: " ++ showStrList (map show $ leaving a)
           acq | acquired a == [] = OList []
             | otherwise = OString $  "    + acquired: " ++ showStrList (map formatTitle $ acquired a)
           lst | lost a == [] = OList []
             | otherwise = OString $  "    + lost: " ++ showStrList (map formatTitle $ lost a)
instance Markdown AugmentedAdvancement where
   printMD a = indentOList $ OList
       [ OString $ name a
       , OList $ map OString $ narrative a 
       , OList $ map OString $ comment a 
       , OList $ map (OString . ("Uses "++) . formatTitle ) $ bookUsed a
       , chnl
       , infl
       , OList $ map (OString . show) $ validation a
       ]
      where inf = sortTraits $ changes $ inferredAdv a
            chn = sortTraits $ changes $ explicitAdv a
            chnl | chn == [] = OList []
                 | otherwise = OList [ OString "Changing traits", OList $ map printMD chn ]
            infl | inf == [] = OList []
                 | otherwise = OList [ OString "Inferred traits", OList $ map printMD inf ]

usesString :: AdvancementLike a => a -> OList
usesString a | u == [] = OList []
             | otherwise = OList [ OString $ "Uses: " ++ showStrList u ]
         where u = usesBook a



instance Markdown Advancement where
   printMD a = indentOList $ OList
         [ OString $ name a
         , OList $ map OString $ narrative a 
         , OList $ map OString $ comment a 
         , usesString a
         , OList $ map printMD $ changes a
         ]



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
                             , os (spellDescription sp)
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
        [ OList [ OString $ "# " ++ name saga
                , OString ""
                , OList $ map OString $ sagaDesc saga
                , OString ""
                ]
        , OList $ [ OString $ "+ " ++ pagesLink (show x) | x <- stateSeasons saga ] 
        , OList [
          OString "" 
          , OString $ "+ " ++ pagesLink "Annals"
          , OString "" 
        ]
        , OString "+ [JSON Syntax Errors](syntaxcheck.txt) in the character files"
        ]

instance Markdown SagaState where
    printMD saga = OList 
        [ OString $ "# " ++ stateTitle saga ++ " - " ++ show (seasonTime saga)
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

instance Markdown Covenant where
    printMD cov = OList 
        [ OString $ "# " ++ (covName $ covenantConcept cov )
        , OString ""
        , printMD $ covenantConcept cov
        , OString ""
        , printMD $ covenantState cov
        ]
    printSheetMD saga cov = OList 
        [ OString $ "# " ++ (covName $ covenantConcept cov )
        , OString ""
        , printMD $ covenantConcept cov
        , OString ""
        , printSheetMD saga $ covenantState cov
        ]

instance Markdown CovenantConcept where
    printMD cc = OList $ bullets cc ++ fd (covDescription cc)
      where bullets = map OString . map ("+ "++) . covconceptHelper
            fd [] = []
            fd (x:xs) = OString "":OString x:fd xs

covconceptHelper :: CovenantConcept -> [ String ]
covconceptHelper cc = filterNothing 
   [ covConcept cc
   , fmap ( ("**Founded** "++) . show ) (covFounded cc)
   , fmap  ("**Appearance** "++)  (covAppearance cc)
   ]

formatTitle :: Book -> String
formatTitle book = tis ++ aus ++ dat
     where aut = trim $ originalAuthor book
           aus | aut == "" = ""
               | otherwise = " by " ++ aut
           tit = trim $ originalTitle book
           tis | tit == "" = ""
               | otherwise = "*" ++ tit ++ "*"
           dat = " (" ++ show (originalDate book) ++ ")"

instance Markdown Book where
    printMD book = OList  
         [ OString $ formatTitle book
         , OList $ [ OString $ showStrList $ map show (bookStats book) 
                 , cnt
                 , lns ]
                 ++ ans ++
                 [ OString $ "Key: " ++ bookID book ++ " (" ++ originalID book ++")" ]
         ]
         where ans = map ( f . trim ) $ bookAnnotation book
               f "" = OList []
               f s = OString s
               lng = trim $ fromMaybe "" $ bookLanguage book
               lns | lng == "" = OList []
                   | otherwise = OString $ "in " ++ lng
               cnt | bookCount book == 1 = OList []
                   | otherwise = OString $ show (bookCount book) ++ " copies"

instance Markdown CovenantState where
    printMD cov = OList  
        [ OString $ "## " ++ (show $ covTime cov)
        , OString ""
        , OString "### Library"
        , OString ""
        , OList $ map indentOList $ map printMD $ library cov
        ]
    printSheetMD saga cov = OList  
        [ OString $ "## " ++ (show $ covTime cov)
        , OString ""
        , characterIndex $ covenFolk saga cov
        , OString ""
        , OString "### Library"
        , OString ""
        , OList $ map indentOList $ map (printSheetMD saga) $ library cov
        ]

-- |
-- = Lab Markdown

instance Markdown Lab where
   printMD lab = indentOList $ OList 
       [ OString $ name lab
       , OList 
         [ OString $ "Refinement: " ++ showBonus (labRefinement $ labState lab)
         , OString $ "Size: " ++ showBonus (labSize $ labState lab)
         , OString $ "Unused size: " ++ showBonus ( (labSize $ labState lab) - usedSize lab )
         , OString $ "Aura: " ++ showBonus (labAura $ labState lab)
         , OString "Stats"
         , OList $ map printMD $ totalBonus lab
         , OString "Description"
         , OList $ map OString $ narrative lab
         , OList $ map OString $ comment lab
         , OString "Virtues and Flaws"
         , foldOList $ OList $ map printMD $ labVirtues $ labState lab
         ]
       ]
instance Markdown LabVirtue where
   printMD v = OList [ OString $ name v
                   , OList $ map OString $ narrative v
                   , OList $ map OString $ comment v
                   ]
instance Markdown LabBonus where
   printMD (LabBonus x "" z) = OString $ x ++ " " ++ showBonus z
   printMD (LabBonus _ y z) = OString $ y ++ " " ++ showBonus z
