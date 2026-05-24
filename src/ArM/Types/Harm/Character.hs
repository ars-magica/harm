{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Harm.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Types to represent Characters and functions for advancement.
--
-- This module contains types to process characters, including 
-- persistence in JSON and advancement.
--
-----------------------------------------------------------------------------
module ArM.Types.Harm.Character where

import GHC.Generics
import Data.Aeson

import Data.KeyPair

import ArM.Types.Advancement
import ArM.Trait
import ArM.Story
import ArM.Helper


-- |
-- = Character

-- | The Character object includes both state information and
-- timeless concept information, as well as the advancements
-- defining the evolution through states.
data Character = Character 
    { charID :: String              -- ^ character ID, used to cross-reference
    , concept :: CharacterConcept   -- ^ concept is the timeless features of the character
    , charTime :: SeasonTime        -- ^ last season advanced
    , memberOf :: Maybe String      -- ^ covenant ID where the character resides
    , traits :: [ Trait ]           -- ^ traits is the main state of the character
    , entryTime :: SeasonTime       -- ^ First season the character is in play
    , pregameDesign :: [ Augmented Advancement ]    -- ^ chargen already processed
    , pregameAdvancement :: [ Advancement ]        -- ^ chargen left to process
    , pastAdvancement :: [ Augmented Advancement ]  -- ^ past advancement (in game), most recent first
    , futureAdvancement :: [ Advancement ]         -- ^ future advancement (in game), next one firstk
    }  deriving (Eq,Generic)

instance HarmObject Character 
instance StoryObject Character where
    name = fullConceptName . concept

-- | Default (empty) character object.
defaultCharacter :: Character 
defaultCharacter = Character { charID = "N/A"
                             , concept = defaultConcept
                             , charTime = NoTime
                             , memberOf = Nothing
                             , traits = [ ]
                             , entryTime = NoTime
                             , pregameDesign = [ ]
                             , pregameAdvancement = [ ]
                             , pastAdvancement = [ ]
                             , futureAdvancement = [ ]
                             }  

instance Show Character where
   show = show . concept 

instance ToJSON Character where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> Character
        <$> v .: "charID"
        <*> v .: "concept"
        <*> fmap parseSeasonTime ( v .:? "season" )
        <*> v .:? "memberOf" 
        <*> fmap maybeList ( v .:? "traits" )
        <*> v .:? "entryTime" .!= NoTime
        <*> v .:? "pregameDesign" .!= []
        <*> v .:? "pregameAdvancement" .!= []
        <*> v .:? "pastAdvancement" .!= []
        <*> v .:? "futureAdvancement" .!= []


-- == Name and identity

instance KeyObject Character where
    harmKey = CharacterKey . charID

-- | Return the name of the character as a string, including house affiliation
-- if defined.
fullConceptName :: CharacterConcept -> String
fullConceptName c = charName c ++ (f $ house c)
      where f Nothing = ""
            f (Just x) | take 2 x == "ex" = " " ++ x
                       | otherwise  = " ex " ++ x

instance Timed Character where
    season = charTime 

-- |
-- = CharacterConcept


-- | The CharacterType distinguishes between Magus, Companion, and Grog.
-- One may want to extend it for magic and faerie charaacters as well as
-- NPCs.
data CharacterType = Magus | Companion | Grog
       deriving (Eq,Generic,Show)
instance ToJSON CharacterType
instance FromJSON CharacterType


-- | The CharacterConcept is the timeless information about the character.
data CharacterConcept = CharacterConcept 
         { charName :: String
         , charType :: CharacterType     -- ^ Magus, Companion or Grog. 
         , briefConcept :: Maybe String
         , quirk :: Maybe String
         , appearance :: Maybe String
         , born :: Maybe Int             -- ^ Year of Birth
         , player :: Maybe String        -- ^ player responsible for the character
         , house :: Maybe String         -- ^ Hermetic House or similar affiliation
         , portrait :: Maybe String      -- ^ image file name (portrait)
         , charGlance :: KeyPairList
         , charData :: KeyPairList
       }  deriving (Eq,Generic)

-- | Default (empty) character concept object.
defaultConcept :: CharacterConcept 
defaultConcept = CharacterConcept { charName = "John Doe"
                                  , charType = Magus
                                  , briefConcept = Nothing
                                  , quirk = Nothing
                                  , appearance = Nothing
                                  , born = Nothing
                                  , player = Nothing
                                  , house = Nothing
                                  , portrait = Nothing
                                  , charGlance = KeyPairList []
                                  , charData = KeyPairList []
       }  

instance ToJSON CharacterConcept where
    -- For efficiency - Not required
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CharacterConcept where
    parseJSON = withObject "CharacterConcept" $ \v -> CharacterConcept
        <$> v .: "name"
        <*> v .: "charType"
        <*> v .:? "concept"
        <*> v .:? "quirk"
        <*> v .:? "appearance"
        <*> v .:? "born"
        <*> v .:? "player"
        <*> v .:? "house"
        <*> v .:? "portrait"
        <*> v .:? "charGlance" .!= KeyPairList []
        <*> v .:? "charData"   .!= KeyPairList []

instance Show CharacterConcept where
   show c = fullConceptName c ++ "\n"
         ++ ( show $ charGlance c ) ++ ( show $ charData c )

