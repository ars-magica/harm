-- |
-- Module      :  ArM.IO.Read
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to read and process ArM files.
--
-- This module is an internal module used by `ArM.IO`.
--
-- This module is an internal module used by `ArM.IO`.
module ArM.IO.Read where

import Data.Aeson (FromJSON)
import Data.Aeson.Generic (readObject)

import qualified Data.Map as M

import Data.Maybe 

import ArM.Story
import ArM.Saga
import ArM.Types.Harm
import ArM.Trait
import ArM.Types.Advancement
-- import ArM.Story
import ArM.DB
import ArM.Helper

import ArM.Debug.Trace

-- * Read Saga Files
-- 
-- | Read a saga from JSON.  Return Maybe SagaFile.
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile = readObject

-- | Load constituent objects for a saga.
loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readDB $ spellFile saga
   wdb <- readCSV $ weaponFile saga
   adb <- readCSV $ armourFile saga
   cs <- mapM readArM $ characterFiles saga
   cov <- mapM readArM ( covenantFiles saga )
   return $ trace "Saga" $ Saga
          { sagaFile = saga
          , seasonTime = GameStart
          , covenants = toDB $ filterNothing cov 
          , characters = toDB $ filterNothing cs 
          , baseURL = Nothing
          , spells = db 
          , weaponsDB = fromJust wdb
          , armourDB = fromJust adb
          }



-- ** Read Character and Covenant Data

-- | The `ReadArM` class wraps loading of an object, loading of constituent
-- files, and pregame advancement into one operaion.
class (Advance t, FromJSON t) => ReadArM t  where
    -- | Load constituent files, such as library CSV files for Covenants.
    loadArM :: Maybe t -> IO (Maybe t)
    loadArM = return

    -- | Read a character from JSON.  Return Maybe Character
    readArM :: String -- ^ Filename
            -> IO (Maybe t)
    readArM fn = readObject fn >>= loadArM >>= return . prepMaybe 
          where prepMaybe Nothing = Nothing
                prepMaybe (Just x) = Just $ prepare x

instance ReadArM Character
instance ReadArM Covenant where
    loadArM Nothing  =  return Nothing
    loadArM (Just cov) = loadCov1 cov >>= loadCov2 >>= return . Just


loadCov1 :: Covenant -> IO (Covenant)
loadCov1 cov = mapM loadCovAdvancement (futureCovAdvancement cov)
         >>= return . ( \x -> cov { futureCovAdvancement = x } )
loadCov2 :: Covenant -> IO (Covenant)
loadCov2 cov = mapM loadCovAdvancement (covenantDesign cov)
         >>= return . ( \x -> cov { covenantDesign = x } )

loadCovAdvancement :: CovAdvancement -> IO (CovAdvancement)
loadCovAdvancement ad = loadCovAdvancement' (bookcsv ad) ad 

loadCovAdvancement' :: Maybe String -> CovAdvancement -> IO (CovAdvancement)
loadCovAdvancement' Nothing ad = return ad
loadCovAdvancement' (Just fn) ad = readBookCSV fn 
      >>= return . ( \ r -> ad { acquired = acquired ad ++ r } ) . wrapBooks

