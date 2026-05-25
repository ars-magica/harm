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
-- This will hopefully be easier to program than the existing
-- 'OList' approach.  The 'fromHList' function converts to 'OList',
-- so existing IO functions can be used.
--
-----------------------------------------------------------------------------
module ArM.Markdown.HOutput where

import Data.HList
import Data.OList
import Data.KeyPair
import ArM.Markdown.Spell
import ArM.Markdown.Possession
import ArM.Markdown.HList
import ArM.Markdown.VF
import ArM.Sheet.Library
import ArM.Trait
import ArM.Types.Advancement
import ArM.Types.Harm
import ArM.Story
import ArM.Saga
import ArM.Helper
import Control.Monad.State.Lazy
import Data.Maybe
-- import ArM.Debug.Trace


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
   -- | Default implementation of `printMD` in the markdown class.
   defaultMD :: h -> OList
   defaultMD = fromMaybe (OString "") . fmap fromHList . printH
   -- | Default implementation of `printSheetMD` in the markdown class.
   defaultSheetMD :: Saga -> h -> OList
   defaultSheetMD saga x = fromMaybe (OList []) $ fmap fromHList 
                         $ evalState ( printS x ) saga

instance HOutput Saga where
   printH saga = Just $ HList ( "# " ++ name saga ) ( hs1:hs2:(ts1 ++ ts2) )
      where hs1 = paragraphsH $ map italic $ narrative saga
            hs2 = paragraphsH $ comment saga
            ts1 = [ hlist $ lnk x | x <- reverse $ advSeasons saga ] 
            ts2 = [ hlist $ lnk GameStart 
                  , hlist $ "+ " ++ "[](0001_Annals)"
                  ]
            lnk x = "+ " ++ "[](" ++ (showKey x) ++ "/index)"

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
--

instance HOutput CovenantConcept where
    printH cc = Just $ HList "" $ hs1:hs2:hs
      where hs = ( map hlist . map ("+ "++) . covconceptHelper ) cc
            hs1 = paragraphsH $ map italic $ narrative cc
            hs2 = paragraphsH $ comment cc

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


-- | Render some of the details for a `CovenantConcept`
covconceptHelper :: CovenantConcept -> [ String ]
covconceptHelper cc = filterNothing 
   [ covConcept cc
   , fmap ( ("**Founded** "++) . show ) (covFounded cc)
   , fmap  ("**Appearance** "++)  (covAppearance cc)
   ]

-- * More basic concepts

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
