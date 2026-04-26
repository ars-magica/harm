---------------------------------------------------------------------------- -- |
-- Module      :  ArM.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities to parse ArMChar files and generate
-- markdown sheets.
--
-- The functions in this module generally take a filename (String)
-- and returns a Maybe object by attempting to parse the relevant 
-- file.
--
-- Some objects, like the Spell Database, are stored in CSV, others
-- in JSON or YAML, including Character and Covenant.
--
-----------------------------------------------------------------------------
module ArM.IO (readSaga,writeSaga) where

import Data.Maybe 
import Data.Aeson (FromJSON)
import Data.Aeson.Generic (readObject)

import System.Directory

import ArM.Advancement
import ArM.Markdown
import ArM.Types.Character
import ArM.Cov.Covenant
import ArM.Types.Saga
import ArM.Types.Possession
import ArM.Types.Advancement
import ArM.Types
import ArM.DB.CSV
import ArM.DB.Weapon()
import ArM.SeasonDisplay
import Data.OList
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Read Saga Files
-- | Read a saga from JSON.  Return Maybe SagaFile.
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile = readObject


-- | Load the saga and all its constituent objects from the given file.
--
-- This calls both `readSagaFile` and `loadSaga`.
-- Characters and covenants are loaded and advanced.
-- The `Saga` object contains a list of characters and one of convenants.
readSaga :: String -- ^ Filename
         -> IO (Maybe Saga)
readSaga fn = readSagaFile fn >>= passMaybe loadSaga


-- | Load constituent objects for a saga.
loadSaga :: SagaFile -> IO Saga
loadSaga saga = do
   db <- readDB $ spellFile saga
   wdb <- readDB $ weaponFile saga
   adb <- readDB $ armourFile saga
   cs <- mapM readArM $ characterFiles saga
   cov <- mapM readArM ( covenantFiles saga )
   return
     $ Saga { sagaFile = saga
           , sagaState = SagaState
              { stateTitle = title saga
              , covenants =  filterNothing cov  
              , characters = filterNothing cs  
              , seasonTime = GameStart
              }
           , baseURL = Nothing
           , spells = fromJust db 
           , weaponsDB = fromJust wdb
           , armourDB = fromJust adb
           }

writeSagaState :: Saga -> IO ()
writeSagaState saga = 
   createDirectoryIfMissing True dir >>
   writeOList (dir ++ "index.md") (printMD st) >>
   writeObjects dir saga (characters st) >>
   writeObjects dir saga (covenants st)
       where dir = rootDir saga ++ fn ++ "/"
             fn = showKey st
             st = sagaState saga

writeSagaStates :: [Saga] -> IO ()
writeSagaStates [] = return ()
writeSagaStates (x:xs) = writeSagaState x >> writeSagaStates xs

writeSagaAnnals :: Saga -> IO ()
writeSagaAnnals saga = writeOList fn $ ann saga
    where fn = rootDir saga ++ "/0001_Annals.md"
          ann = OList . (OString "# Annals":) . map printMD . sagaAnnals . sagaState

-- |
-- Write markdown files for the saga and all its covenants and characters.
writeSaga :: Saga -> IO ()
writeSaga saga = do
   writeOList (rootDir saga ++ "/index.md") $ printMD saga

   let sagas = advanceSaga saga

   writeSagaStates sagas
   writeSagaAnnals (head sagas)
   return () 

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
      >>= return . ( \ r -> ad { acquired' = acquired' ad ++ r } ) . wrapBooks

-- |
-- = Write Character Sheets

-- | Write charactersheets in MarkDown
-- File name is derived from the character name.
writeObjects :: (HarmObject h, Markdown h) 
             => String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose objects are written
             -> [ h ]   -- ^ List of objects to write
             -> IO ()
writeObjects dir saga cs = mapM wf  cs >> return ()
         where wf c = (writeOList (fn c) $ printSheetMD saga c)
               fn c = dir ++ "/" ++ stateName c ++ ".md"
