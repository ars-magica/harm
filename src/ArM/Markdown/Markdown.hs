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
                    , enchantedMD  -- Unused
                    ) where

import Data.Maybe 
import Control.Monad

import ArM.Markdown.Possession 
import ArM.Markdown.Spell
import ArM.Markdown.VF
import ArM.Character 
import ArM.Saga
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
                , OList $ map printMDaa as
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
-- line/paragraph.  This works for any instance of 'Show'.
showlistMD :: Show a => String -> [a] -> OList
showlistMD _ [] = OList []
showlistMD s xs = OList [ OString s
                        , toOList $ (map (++", ") $ map show xs)
                        ]
 
-- = Markdown for the Character types
 
-- |
-- The 'CharacterConcept' is set as a description list.
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
               , OString $ ": " ++ ( fromMaybe "---" $ quirk c )
               , OString ""
               , OString "Appearance" 
               , OString $ ": " ++ ( fromMaybe "---" $ appearance c )
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


instance Markdown MagicEffect  where
   printMD = fromHList . effectH 
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
   printMD = fromHList . printPossessionH

-- | Make a list of possessions excluding books and labtexts in Markdown.
listPossessions :: [ Possession ] -> OList
listPossessions ps = OList
      [ OString "#### Mundane Equipment"
      , indentOList $ OList [ OString "Silver"
                            , (pList $ filter ( (/=0) . silver ) ps)
                            , (pList $ filter ( (/=0) . silverYield ) ps)
                            , OString "Weapons"
                            , (pList ws)
                            , OString "Armour"
                            , (pList as)
                            , OString "Equipment"
                            , (pList es)
                            -- Silver
                            ]
      , OString "#### Magic Gadgets"
      , indentOList $ OList [ OString "Vis"
                            , (pList vs)
                            , OString "Vis sources"
                            , (pList $ filter isVisSrc ps)
                            , OString "Arcane Connections"
                            , (pList acs)
                            , OString "Magic Items"
                            , (pList ms)
                            ]
      ]
   where vs = filter isVis ps
         ws = filter isWeapon ps
         as = filter isArmour ps
         acs = filter isAC ps
         ms = filter isMagic ps
         es = filter isMundaneEquipment ps
         -- pList :: [ Possession ] -> OList
         pList = foldOList . OList  . map printMD . sortTraits 

-- | Set a header line followed by a bullet list
bulletWithHeader :: Markdown a => String -> [a] -> OList
bulletWithHeader _ [] = OList []
bulletWithHeader h xs = OList [ OString h, f xs ]
         where f = indentOList . foldOList . OList . map printMD 

instance Markdown Library where
   printMD lib = OList [ OString ("# " ++ name lib)
                       , OString ""
                       , OString $ "+ updated after " ++ (show $ season lib)
                       , bulletWithHeader "## Antologies" (antologies lib )
                       , bulletWithHeader "## Arts" (artBooks lib )
                       , bulletWithHeader "## Abilities" (abilityBooks lib )
                       , bulletWithHeader "## Other works" (otherBooks lib )
                       , bulletWithHeader "## Grimoires" (grimoires lib )
                       , bulletWithHeader "## Spell Lab Texts" (spellTexts lib )
                       , bulletWithHeader "## Enchantment Lab Texts" (itemTexts lib )
                       ]

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
               , OString $ "+ Ceremonial Casting Bonus: " ++ showSigned (ceremonialCastingBonus c)
               , OString ""
               , OString "## Laboratory"
               , OString ""
               , toOList $ printLabTotals c
               , OString ""
               , OString "*Lab totals include aura, general quality, and lab art specialisations, but no activity bonuses, apprentices, or familiars.*"
               ]
   printSheetMD saga c' = OList 
               [ briefTraits c
               , showlistMD "+ **Characteristics:** "  $ sortTraits $ charList c
               , showlistMD "+ **Personality Traits:** "  $ sortTraits $ ptList c
               , showlistMD "+ **Reputations:** "  $ sortTraits $ reputationList c
               , showlistMD "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
               , indentOList $ OList $ [ OString "**Abilities:**"
                        , OList (map (OString . show) ( sortTraits $ abilityList c )) ]
               , listPossessions $ possessionList c
               -- , indentOList $ OList $ [ OString "**Possessions:**"
                        -- , OList (map (OString . show) ( sortTraits $ possessionList c )) ]
               , OString ""
               , printCombatMD saga c
               , mag
               ]
         where c = addCastingScores (spells saga) c'
               mag | isMagus c' = OList 
                       [ artVisMD c
                       , OString ""
                       , printFullGrimoire (spells saga) $ sortTraits $ spellList c 
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

instance Markdown CharacterState where
   printMD c = OList
       [ OString $ "## Sheet " ++ (show $ gameSeason c )
       , OString ""
       , printMD $ characterSheet c
       ]
   printSheetMD saga c = OList
       [ OString $ "## Character Sheet " ++ (show $ gameSeason c) 
       , OString ""
       , printSheetMD saga $ characterSheet c
       ]

-- |
-- == Markdown for Age, Confidence, Warping, and Decrepitude

-- | Print age, confidence, warping, and decrepitude as bullet points
briefTraits :: CharacterSheet -> OList
briefTraits c = OList
          [ printAge c
          , OList $ map printMD $ confList c
          , OList $ map printMD $ otherList c
          ]
printAge :: CharacterSheet -> OList
printAge c | isNothing ag' = OString "**Age** undefined"
         | otherwise = OString $ "+ **Age:** " ++ show yr ++ " years (apparent age " 
            ++ show (yr - apparentYounger ag)  ++ ") Aging Bonus: " ++ showSigned b
            ++ " (" ++ (showStrList $ map f bs) ++ ")"
   where ag' = ageObject c
         ag = fromJust ag'
         yr = ageYears ag
         f (x,y) = x ++ " " ++ showSigned y
         bs = charAgingBonusList c
         b = charAgingBonus c

instance Markdown Age where
   printMD c = OString $ "+ **Age:** " ++ show y ++ " years (apparent age " 
            ++ show (y - apparentYounger c)  ++ ")" ++ lr
      where y = ageYears c
            lrs = longevityRitual c
            lr | lrs < 0 = ""
               | otherwise = " Longevity Ritual: " ++ show lrs

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
-- instance AdvancementLike t => Markdown (Augmented t) where
printMDaa :: Augmented Advancement -> OList
printMDaa a' = indentOList $ OList $ storyOList a ++
       [ OString $ "Uses "++ (show $ bookRead a)
       , chnl
       , infl
       , OList $ map (OString . show) $ validation a'
       ]
      where inf = sortTraits $ changes $ inferredAdv a'
            chn = sortTraits $ changes $ explicitAdv a'
            a = contractAdvancement a'
            chnl | chn == [] = OList []
                 | otherwise = OList [ OString "Changing traits", OList $ map printMD chn ]
            infl | inf == [] = OList []
                 | otherwise = OList [ OString "Inferred traits", OList $ map printMD inf ]

usesString :: Advancement -> OList
usesString a | u == "" = OList []
             | otherwise = OList [ OString $ "Uses: " ++ u ]
         where u = show $ readsBook a

instance Markdown Advancement where
   printMD a = indentOList $ OList $ storyOList a ++
         [ usesString a
         , OList $ map printMD $ changes a
         ]

-- ** Pretty print arts

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


-- | Auxiliary for 'artMD', rendering a single line in the table
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

-- | Auxiliar for 'artVisMD'' rendering the body of the table.
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
printCombatMD :: Saga -> CharacterSheet -> OList
printCombatMD saga cs = OList x
    where tab = computeCombatStats ( weaponsDB saga ) cs
          x | tab == [] = []
            | otherwise = [ combatHead, combatBody tab ]

-- | Set the table body for 'printCombatMD'
combatBody :: [CombatLine] -> OList
combatBody = OList . map combatBodyLine

-- | Set a single line for 'printCombatMD'
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

-- | Set the header for 'printCombatMD'
combatHead :: OList
combatHead = OList [ OString "| Weapon | Init | Atk | Def | Dam | Range | Load | Comment |"
                   , OString "|  :- |  -: |  -: |  -: |  -: |  -: |  -: | :- |"
                   ]


-- = Saga Markdown

instance Markdown Saga where
    printMD saga = OList 
        [ OList [ OString $ "# " ++ name saga
                , OString ""
                , OList $ map OString $ sagaDesc saga
                , OString ""
                ]
        , OList ( [ OString $ "+ " ++ "[](" ++ (showKey x) ++ "/index)" | x <- advSeasons saga ] 
                ++ [ OString $ "+ " ++ "[](" ++ showKey GameStart ++ "/index)" ] )
        , OList [
          OString "" 
          , OString $ "+ " ++ "[](0001_Annals)"
          , OString "" 
        ]
        ]

instance Markdown SagaState where
    printMD saga = OList 
        [ OString $ "# " ++ stateTitle saga ++ " - " ++ show (gameSeason saga)
        , OString ""
        , characterIndex $ characters saga
        , OString ""
        , covenantIndex $ covenants saga
        , OString ""
        , OString "## Advancement Errors"
        , OString ""
        , indentOList $ foldOList $  advancementErrors saga
        , OString ""
        , OString "## Advancement Notices"
        , OString ""
        , indentOList $ foldOList $ advancementNotices saga
        ]

-- |
-- = Covenant Markdown

instance Markdown Covenant where
    printMD cov = OList 
        [ OString $ "# " ++ (name cov )
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
        , printCovenantStateMD saga cov
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


instance Markdown Book where
    printMD = fromHList . printBookH

instance Markdown CovenantState where
    printMD cov = OList  
        [ OString $ "## Updated" ++ (show $ covTime cov)
        , OString ""
        , OString "### Boons and Hooks"
        , OString ""
        , OList $ map ( indentHList . vfH ) ( boonhook cov )
        , OString ""
        , OString "### Possessions"
        , OString ""
        , listPossessions $ possessions cov
        , OString ""
        ]

-- | Print the covenant state of the given covenant
printCovenantStateMD :: Saga -> Covenant -> OList
printCovenantStateMD saga cov = OList  
        [ OString $ "## " ++ (show $ season cov)
        , OString ""
        , characterIndex $ fmaybe (covenFolk saga) (covenantState cov)
        , OString ""
        , OString (pagesLink $ stateName $ getLibrary cov)
        , OString ""
        , OString "### Boons and Hooks"
        , OString ""
        , OList $ map ( indentHList . vfH ) ( fmaybe boonhook $ covenantState cov )
        , OString ""
        , OString "### Possessions"
        , OString ""
        , listPossessions $ fmaybe possessions (covenantState cov)
        ]

-- | Apply a function returning a list on a maybe object, returning the empty
-- list if the object is Nothing.
fmaybe :: ( a -> [b] ) -> Maybe a -> [b]
fmaybe x = fromMaybe [] . fmap x


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
   printMD (LabBonus x "" z) = OString $ x ++ " " ++ showBonus z
   printMD (LabBonus _ y z) = OString $ y ++ " " ++ showBonus z

-- * Convenience Functions

storyOList :: StoryObject a => a -> [ OList ]
storyOList ob = 
       [ OString $ name ob
       , OList  $ map italicOString $ narrative ob
       , OList  $ map OString $ comment ob 
       ]
