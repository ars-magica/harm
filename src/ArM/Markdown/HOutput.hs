{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Markdown.HOutput
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Rendering data as 'HList' objects.
--
-----------------------------------------------------------------------------
module ArM.Markdown.HOutput where

import Data.HList
import Data.KeyPair
import ArM.DB
import ArM.Markdown.Magus
import ArM.Markdown.Spell
import ArM.Markdown.Saga
import ArM.Markdown.Possession
import ArM.Markdown.HList
import ArM.Markdown.VF
import ArM.Sheet.Library
import ArM.Trait
import ArM.Character
import ArM.Types.Harm
import ArM.Story
import ArM.Saga
import ArM.Helper
import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import ArM.Debug.Trace


-- * The class

-- | `HOutput` provides the API to render objects in Markdown using the `HList`
-- representation.
class HOutput h where
   -- | Render an object in 'HList' format allowing markdown notation.
   printH :: h -> Maybe HList
   -- | Render an object in 'HList' format allowing markdown notation.
   --
   -- The monadic version gives access to the databases in the `Saga` object
   -- to look up details.
   --
   -- The default implementation ignores the monad and is equivalent to `printH`.
   printS :: h -> State Saga (Maybe HList)
   printS = return . printH

instance HOutput Saga where
   printH saga = Just $ HList ( "# " ++ name saga ) ( hs1:hs2:(ts1 ++ ts2) )
      where hs1 = paragraphsH $ map italic $ narrative saga
            hs2 = paragraphsH $ comment saga
            ts1 = [ hlist $ lnk x | x <- reverse $ advSeasons saga ] 
            ts2 = [ hlist $ lnk GameStart 
                  , hlist $ "+ " ++ "[](0001_Annals)"
                  ]
            lnk x = "+ " ++ "[](" ++ (showKey x) ++ "/index)"

-- * Character


instance HOutput Character where
   printH  c = trace "Unsupport - brief Character sheeet"
            $ Just $ HList "" $ filterNothing
            [ printH $ concept  c 
            , hheader $ "## Sheet " ++ (show $ gameSeason c )
            , Just $ sheetH c
            , designH c
            , Just $ advancementH c
            ]
   printS c = get >>= return . characterH c

characterH :: Character -> Saga -> Maybe HList
characterH c saga = Just $ HList "" $ filterNothing
            [ printH $ concept c
            , Just $ sheetSheetH c 
            , adv
            , Just $ combatSheetH c saga
            , magusSheetH c saga
            ]
        where adv | isGameStart c = designH c
                  | otherwise = Just $  advancementH c

-- | Render a list of objects as a comma-separated list on a single
-- line/paragraph.  This works for any instance of 'Show'.
showlistH :: Show a => String -> [a] -> Maybe HList
showlistH _ [] = Nothing
showlistH s xs = jhlist $ s ++ ( foldr (++) "" $ (map (++", ") $ map show xs) )

printAgeH :: Character -> HList
printAgeH c | isNothing ag' = hlist "**Age** undefined"
         | otherwise = hlist $ "+ **Age:** " ++ show yr ++ " years (apparent age " 
            ++ show (yr - apparentYounger ag)  ++ ") Aging Bonus: " ++ showSigned b
            ++ " (" ++ (showStrList $ map f bs) ++ ")"
   where ag' = ageObject c
         ag = fromJust ag'
         yr = ageYears ag
         f (x,y) = x ++ " " ++ showSigned y
         bs = charAgingBonusList c
         b = charAgingBonus c

briefTraitsH :: Character -> Maybe HList
briefTraitsH c = Just $ HList "" $ filterNothing
          [ Just $ printAgeH c
          , Just $ HList "" $ filterNothing $ map printH $ confList c
          , Just $ HList "" $ filterNothing $ map printH $ otherList c
          , showlistH "+ **Characteristics:** "  $ sortTraits $ charList c
          , showlistH "+ **Personality Traits:** "  $ sortTraits $ ptList c
          , showlistH "+ **Reputations:** "  $ sortTraits $ reputationList c
          , showlistH "+ **Virtues and Flaws:** "  $ sortTraits $ vfList c
          ]

sheetH :: Character -> HList
sheetH c = HList "" $ filterNothing
               [ briefTraitsH c
               , showlistH "+ **Abilities:** "  $ sortTraits $ abilityList c
               , showlistH "+ **Arts:** "  $ sortTraits $ artList c
               , showlistH "+ **Spells:** "  $ sortTraits $ spellList c
               , showlistH "+ **Possessions:** "  $ sortTraits $ characterPossessions c
               -- , toOList $ printCastingTotals c
               , jhlist ""
               , jhlist $ "+ Ceremonial Casting Bonus: " ++ showSigned (ceremonialCastingBonus c)
               , jhlist "## Laboratory"
               --j , toOList $ printLabTotals c
               , jhlist ""
               , jhlist "*Lab totals include aura, general quality, and lab art specialisations, but no activity bonuses, apprentices, or familiars.*"
               ]


-- | Set a list of spells.
-- Each spell is set using 'spellMD', and the result is indented as a
-- hierarchical list.
sheetSheetH :: Character -> HList
sheetSheetH c = HList ( "## Character Sheet " ++ (show $ gameSeason c) ) 
               $ filterNothing
               [ briefTraitsH c
               , Just $ indentList $ HList "**Abilities:**"
                        (map (hlist . show) ( sortTraits $ abilityList c ))
               , Just $ listPossessionsH $ characterPossessions c
               , jhlist ""
               ]
combatSheetH  :: Character -> Saga -> HList
combatSheetH  c saga = printCombatH saga c

magusSheetH :: Character -> Saga -> Maybe HList
magusSheetH c saga
   | isMagus c = Just $ HList "" 
               [ artVisH c
               , hlist ""
               , printFullGrimoireH (spells saga) 
                  $ sortTraits $ spellsWithScores (spells saga) c 
               , hlist ""
               , HList "" $ map hlist $ printCastingTotals c 
               , hlist ""
               , hlist $ "+ Ceremonial Casting Bonus: " 
                       ++ showSigned (ceremonialCastingBonus c)
               , hlist ""
               , hlist "## Laboratory"
               , hlist ""
               , HList "" $ map hlist $ printLabTotals c 
               , hlist ""
               , fromMaybe (hlist "") $ printH $ characterLab c
               ]
   | otherwise = Nothing 

-- | Set a list of spells.
-- Each spell is set using 'spellMD', and the result is indented as a
-- hierarchical list.
printFullGrimoireH :: SpellDB -> [Spell] -> HList
printFullGrimoireH db xs = HList "## Grimoire" 
                         [ hlist ""
                         , HList "" $ map (indentList . spellDescH) ys 
                         , hlist ""
                         , hlist $ "Total: " ++show (totalLevels xs)  
                            ++ " levels of spells."
                         ]
   where ys = [ (x,f x) | x <- xs ]
         f x = spellTRecord x `mplus` spellLookup (traitKey x) db 



instance HOutput CharacterConcept where
   printH = conceptPrintH "../images/"
   printS cov = get >>= return . fromMaybe "../images/" . baseURL 
                    >>= ( \ x -> return ( conceptPrintH x cov ) )

conceptPrintH :: String -> CharacterConcept -> Maybe HList
conceptPrintH dir c = Just $ HList ("# " ++ nm )
               [ img
               , hlist ""
               , dlMaybeH (show (charType c)) ( briefConcept c )
               , dlMaybeH "Quirk" (  quirk c )
               , dlMaybeH "Appearance" (  appearance c )
               , dlMaybeH "Born" ( fmap show $ born c )
               , dlMaybeH "Player" ( player c )
               , fromMaybe (hlist "") ( printH $ charGlance c ) 
               , fromMaybe (hlist "") ( printH $ charData c )
               ]
          where img | isNothing (portrait c) = hlist ""
                    | otherwise = hlist imgfn
                imgfn = ("![" ++ nm ++ "](" ++ dir ++ fromJust (portrait c) ++ ")")
                nm = fullConceptName c 

--
-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
chargenH :: Character -> Maybe HList
chargenH c | as == [] = Nothing
           | otherwise = Just $ HList "## Char Gen Advancements"
                              $ filterNothing $ map printH as
           where as = pregameAdvancement c

-- ** Covenant
--
instance HOutput Covenant where
    printH cov = printCovenant cov Nothing
    printS cov = get >>= f cov >>= return . printCovenant cov
        where f x saga = return $ Just $ characterIndexH $ covenFolk saga x

-- | Render the covenant.
--
-- If the calling function has access to the `Saga`, the list of covenfolk
-- can be rendered with reference to characters from the database.
-- If not, Nothing may be passed.
printCovenant :: Covenant     -- ^ The covenant
              -> Maybe HList  -- ^ List of covenfolk or Nothing
              -> Maybe HList  -- ^ Rendered output
printCovenant cov idx = Just $ HList ( "# " ++ (name cov ) ) $ filterNothing
        [ jhlist ""
        , printH $ covenantConcept cov
        , hheader ( "## Updated " ++ (show $ season cov) ) 
        , idx 
        , jhlist ""
        , jhlist (("+ "++) $ pagesLink $ stateName $ getLibrary cov)
        , hheader "### Boons and Hooks" 
        , Just $ HList "" $ map ( indentList . vfH ) ( boonhook cov )
        , jhlist ""
        , Just $ listPossessionsH $ possessions cov
        ] 

instance HOutput CovenantConcept where
    printH cc = Just $ HList "" $ hs1:hs2:hs
      where hs = ( map hlist . map ("+ "++) . covconceptHelper ) cc
            hs1 = paragraphsH $ map italic $ narrative cc
            hs2 = paragraphsH $ comment cc

-- | Render some of the details for a `CovenantConcept`
covconceptHelper :: CovenantConcept -> [ String ]
covconceptHelper cc = filterNothing 
   [ covConcept cc
   , fmap ( ("**Founded** "++) . show ) (covFounded cc)
   , fmap  ("**Appearance** "++)  (covAppearance cc)
   ]

-- * More basic concepts

-- | Make a list of possessions excluding books and labtexts in Markdown.
listPossessionsH :: [ Possession ] -> HList
listPossessionsH ps = HList "### Possessions"
      [ HList "#### Mundane Equipment" 
      $ f [ printPossessionsH "Silver"
               (filter ( (/=0) . silver ) ps 
               ++  filter ( (/=0) . silverYield ) ps)
          , printPossessionsH "Weapons" $ filter isWeapon ps
          , printPossessionsH "Armour" $ filter isArmour ps
          , printPossessionsH "Equipment" $ filter isMundaneEquipment ps
          ]
      , HList "#### Magic Gadgets" 
      $ f [ printPossessionsH "Vis" $ filter isVis ps
          , printPossessionsH "Vis sources" $ filter isVisSrc ps
          , printPossessionsH "Arcane Connections" $ filter isAC ps
          , printPossessionsH "Magic Items" $ filter isMagic ps
          ]
      ]
   where f = map indentList . filterNothing

instance HOutput Enchantment  where
   printH (LesserItem eff) = printH eff 
   printH (GreaterDevice vn eff) = Just $ HList 
       ( "Greater Enchanted Device (opened with " ++ show vn ++ "p vis)" )
       ( filterNothing $ map printH eff )
   printH (Talisman vn eff) = Just $ HList 
       ( "Talisman (opened with " ++ show vn ++ "p vis)" )
       ( filterNothing $ map printH eff )
   printH (ChargedItem vn eff) = Just $ HList 
       ( "Charged Item (" ++ show vn ++ "charges)" )
       ( filterNothing [ printH eff ] )
   printH MundaneItem = jhlist "Mundane Item" 





instance HOutput Possession  where
   printH = Just . printPossessionH
instance HOutput LabText where
   printH = Just . textH
instance HOutput Book where
    printH = Just . printBookH
instance HOutput SpellRecord where
   printH = Just . spellH
instance HOutput MagicEffect where
   printH = Just . effectH
instance HOutput ProtoTrait where
   printH = jhlist . show 
instance HOutput Trait where
   printH (AgeTrait x) = printH  x
   printH x = jhlist $ show x


instance HOutput Age where
   printH c = Just $ HList h lr
      where y = ageYears c
            lrs = longevityRitual c
            lr | lrs < 0 = []
               | otherwise = [ hlist $ " Longevity Ritual: " ++ show lrs ]
            h = "+ **Age:** " ++ show y ++ " years (apparent age " 
                ++ show (y - apparentYounger c)  ++ ")" 

instance HOutput Lab where
   printH lb = Just $ indentList $ HList ( name lb ) $ filterNothing
         [ jhlist $ "Refinement: " ++ showSigned (labRefinement $ labState lb)
         , jhlist $ "Size: " ++ showSigned (labSize $ labState lb)
         , jhlist $ "Used size: " ++ used ++ " out of " ++ lim
         , jhlist $ "Safety: " ++ saf ++ " (" ++ bas ++ sfl ++ ")"
         , jhlist $ "Aura: " ++ show (labAura $ labState lb)
         , jhlist $ "Traits: " ++ commaList ts
         , jhlist $ "Art Specialisations: " ++ commaList arsp
         , jhlist $ "Activity Specialisations: " ++ commaList acsp
         , jhlist "Description"
         , narrativeH lb
         , commentH lb
         , jhlist ""
         , Just $ HList "Virtues and Flaws" 
                $ filterNothing $ map printH $ labVirtues $ labState lb
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

instance HOutput LabVirtue where
   printH v = Just $ HList (name v) $ filterNothing
                   [ narrativeH v
                   , commentH v
                   , jhlist ts 
                   ]
        where ts = "Bonuses: " ++ commaList (labVirtueBonus v)

instance HOutput LabBonus where
   printH (LabBonus x "" z) = jhlist $ x ++ " " ++ showBonus z
   printH (LabBonus _ y z) = jhlist $ y ++ " " ++ showBonus z

instance HOutput Validation where
   printH (Validated x) = jhlist $ "Validated: " ++ x
   printH (ValidationError x) = jhlist $ "**Error:** " ++ x
   printH (ValidationWarning x) = jhlist $ "*Warning:* " ++ x

instance HOutput Confidence where
   printH c = jhlist $
             "+ **" ++ cname c ++ "**: " ++ show (cscore c) ++ " ("
             ++ show (cpoints c) ++ ")" 
instance HOutput OtherTrait where
   printH c = jhlist $
             "+ **" ++ trait c ++ "**: " ++ show (otherScore c) ++ " ("
             ++ show (otherExcess c) ++ ")" 

-- | Set a header line followed by a bullet list
bulletWithHeaderH :: HOutput a => String -> [a] -> Maybe HList
bulletWithHeaderH _ [] = Nothing
bulletWithHeaderH h xs = Just $ HList h $ filterNothing $ map ( fmap indentList . printH ) xs

instance HOutput Library where
   printH lib = Just $ HList ("# " ++ name lib) $ filterNothing
                       [ Just $ hlist $ "+ Updated after " ++ (show $ season lib)
                       , bulletWithHeaderH "## antologies" (antologies lib )
                       , bulletWithHeaderH "## arts" (artBooks lib )
                       , bulletWithHeaderH "## abilities" (abilityBooks lib )
                       , bulletWithHeaderH "## other works" (otherBooks lib )
                       , bulletWithHeaderH "## grimoires" (grimoires lib )
                       , bulletWithHeaderH "## spell lab texts" (spellTexts lib )
                       , bulletWithHeaderH "## enchantment lab texts" (itemTexts lib )
                       ]

instance HOutput Advancement where
   printH a = Just $ indentList $ HList (name a) $ filterNothing
            [ narrativeH a
            , commentH a
            , usesString 
            , f $ filterNothing $ map printH $ changes a
            ]
         where usesString | u == [] = Nothing
                          | otherwise = jhlist $ "Uses: " ++ showStrList u 
               u = map show $ readsBook a
               f [] = Nothing
               f xs = Just $ HList "" xs

-- | Render the advancement log.
-- This is two lists of past and future advancement objects
advancementH :: Character -> HList
advancementH c = HList "" [ HList "## Past Advancement" $ map augAdvH as
                          , HList "## Future Advancement" $ filterNothing $ map printH bs
                          ]
   where as = pastAdvancement c
         bs = futureAdvancement c

-- | Render the char gen design.
-- This is a list of all the pregame advancement objects.
designH :: Character -> Maybe HList
designH c  | as == [] = Nothing
            | otherwise = Just $ HList "## Game start design" $ map augAdvH as
            where as = pregameDesign c

augAdvH :: Augmented Advancement -> HList
augAdvH a' = indentList $ HList ( name a ) $ filterNothing
           [ narrativeH a
           , commentH a
           , fmap (hlist . ("Reads "++) . name ) $ bookRead a
           , chnl
           , infl
           , Just $ HList "" $ filterNothing $ map printH $ validation a'
           ]
      where inf = sortTraits $ changes $ inferredAdv a'
            chn = sortTraits $ changes $ explicitAdv a'
            a = contractAdvancement a'
            chnl | chn == [] = Nothing
                 | otherwise = Just $ HList "Changing traits" $ filterNothing $ map printH chn 
            infl | inf == [] = Nothing
                 | otherwise = Just $ HList "Inferred traits" $ filterNothing $ map printH inf

-- * Keypair

instance HOutput FieldValue where
   printH = jhlist . show
instance HOutput KeyPair where
   printH (KeyPair x y) = Just $ HList x [ hlist $ ':':' ':show y, hlist "" ]
instance HOutput KeyPairList where
   printH (KeyPairList xs) = Just $ HList "" $ filterNothing $ map printH xs

-- * Derived instances

instance HOutput a => HOutput (Maybe a) where
   printH Nothing = Nothing
   printH (Just x) = printH x
instance HOutput a => HOutput [a] where
   printH [] = Nothing
   printH x = Just $ HList "" $ filterNothing $  map printH x

-- * The Saga State

-- | Render the state page for the Saga
sagaStateH :: Saga -> HList 
sagaStateH saga = HList ( "# " ++ name saga ++ " - " ++ show (gameSeason saga) )
        [ hlist ""
        , characterIndexH $ characterList saga
        , covenantIndexH $ covenantList saga
        , HList "" [ hlist "## Advancement Errors" ]
        , errorsH saga
        , HList "" [ hlist  "## Advancement Warnings"  ]
        , warningsH  saga
        ]


-- ** Combat 

-- | Set the Combat Stats of the Character as an 'OList'
printCombatH :: Saga -> Character -> HList
printCombatH saga cs = HList "" (map hlist $ combatTable tab)
    where tab = computeCombatStats ( weaponsDB saga ) cs

combatTable :: [CombatLine] -> [String]
combatTable xs = combatHead1:combatHead2:combatBody0 xs

-- | Set the table body for 'printCombatMD'
combatBody0 :: [CombatLine] -> [String]
combatBody0 = map combatLine

-- | Set a single line for 'printCombatMD'
combatLine :: CombatLine -> String
combatLine c = "| " ++ (combatLabel c) ++ 
               " | " ++ (show $ combatInit c) ++
               " | " ++ (showstat $ combatAtk c) ++
               " | " ++ (showstat $ combatDef c) ++
               " | " ++ (showstat $ combatDam c) ++
               " | " ++ (showstat $ combatRange c) ++
               " | " ++ (show $ combatLoad c) ++
               " | " ++ (combatComment c) ++
               " |"

-- | Set the header for 'printCombatMD'
combatHead1 :: String 
combatHead1 = "| Weapon | Init | Atk | Def | Dam | Range | Load | Comment |"
combatHead2 :: String 
combatHead2 = "|  :- |  -: |  -: |  -: |  -: |  -: |  -: | :- |"

-- * Errors

-- | Render the validation errors from a saga
errorsH :: Saga -> HList
errorsH = errorsH' isValError
    where isValError (ValidationError _) = True
          isValError _ = False

errorsH' :: (Validation -> Bool) -> Saga -> HList
errorsH' f saga | length msgs == 0 = hlist "No messages"
               | otherwise = HList "" msgs
    where formatOutput (_,_,_,[]) = Nothing
          formatOutput (cid,_,ssn,vs) = Just $ indentList $ 
              HList ( show cid ++ ": " ++ ssn ) 
                    $ (filterNothing $ map printH  vs) 
          errors = errorList saga
          msgs = filterNothing $ map ( formatOutput . g ) errors
          g (x,y,z,vs) = (x,y,z,filter f vs)

-- | Render the validation warnings from a saga
warningsH :: Saga -> HList
warningsH = errorsH' isValError
    where isValError (ValidationWarning _) = True
          isValError _ = False

-- * Advancements

instance (HOutput a, ContractAdvancement a) 
      => HOutput (Augmented a) where
   printH = printH . contractAdvancement
instance HOutput CovAdvancement where
   printH ad = Just $ HList "" $ sls ++ f ch
      where ch = printCovChangesH ad
            sls = filterNothing $ map printH $ caStory ad 
            f [] = []
            f xs = [ HList "Changes" xs ]
instance HOutput Story where
   printH story = Just $ HList ( storyTitle story ++ sq (storySQ story) )
                $ filterNothing [ narrativeH story, commentH story ]
      where sq Nothing = "(no source quality)"
            sq (Just x) = " (SQ " ++ show x ++ ")"
printCovChangesH :: CovAdvancement -> [ HList ]
printCovChangesH a =  filterNothing [ j, lv, acq, lst ] 
     where j | joining a == [] = Nothing
             | otherwise = jhlist $  "joining: " ++ showStrList (map show $ joining a)
           lv | leaving a == [] = Nothing
              | otherwise = jhlist $  "leaving: " ++ showStrList (map show $ leaving a)
           acq | acquired a == [] = Nothing
               | otherwise = jhlist $  "acquired: " ++ showStrList (map name $ acquired a)
           lst | lost a == [] = Nothing
               | otherwise = jhlist $  "lost: " ++ showStrList (map name $ lost a)
