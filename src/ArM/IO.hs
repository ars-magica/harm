-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Simple utilities to read and parse ArMChar files.
--
-- The functions in this module generally take a filename (String)
-- and returns a Maybe object by attempting to parse the relevant 
-- file.
--
-- Some objects, like the Spell Database, are stored in CSV, others
-- in JSON, including Character and Covenant.
--
-----------------------------------------------------------------------------
module ArM.IO (readSaga,writeSaga) where

import Data.Maybe 
import Data.List 
import Data.Aeson 
import qualified Data.ByteString.Lazy as LB
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec

import System.Directory

import ArM.Advancement
import ArM.Markdown
import ArM.Types.Covenant
import ArM.Types.Saga
import ArM.Types
import ArM.DB.CSV
import ArM.DB.Weapon()
import ArM.SeasonDisplay
import ArM.BasicIO
import ArM.Helper

import ArM.Debug.Trace

-- |
-- = Read Saga Files
-- | Read a saga from JSON.  Return Maybe SagaFile
readSagaFile :: String -- ^ Filename
             -> IO (Maybe SagaFile)
readSagaFile fn = LB.readFile fn >>= return . decode

-- | Load the saga and its constituent objects from the given file.
-- This calls both `readSagaFile` and `loadSaga`.
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
   cov <- ( mapM readArM ( covenantFiles saga )
            >>= mapM loadCovenant )
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
           , weapons = fromJust wdb
           , armour = fromJust adb
           }

writeSagaState :: Saga -> IO ()
writeSagaState saga = 
   createDirectoryIfMissing True dir >>
   writeOList (dir ++ "index.md") (printMD st) >>
   writeObjects dir saga (characters st) >>
   writeObjects dir saga (covenants st)
       where dir = rootDir saga ++ fn ++ "/"
             fn = show $ seasonTime st
             st = sagaState saga

writeSagaStates :: [Saga] -> IO ()
writeSagaStates [] = return ()
writeSagaStates (x:xs) = writeSagaState x >> writeSagaStates xs

writeSagaAnnals :: Saga -> IO ()
writeSagaAnnals saga = writeOList fn $ ann saga
    where fn = rootDir saga ++ "/Annals.md"
          ann = OList . (OString "# Annals":) . map printMD . sagaAnnals . sagaState

writeSaga :: Saga -> IO ()
writeSaga saga = do
   writeOList (rootDir saga ++ "/index.md") $ printMD saga

   let sagas = advanceSaga saga

   writeSagaStates sagas
   writeSagaAnnals (head sagas)
   return () 

-- |
-- == Read Character and Covenant Data

-- class FromJSON c => ArMRead c where

-- | Read a character from JSON.  Return Maybe Character
readArM :: (Advance c, FromJSON c)
        => String -- ^ Filename
        -> IO (Maybe c)
readArM fn = LB.readFile fn >>= return . prepMaybe . decode
      where prepMaybe Nothing = trace ("Failed to read "++fn) Nothing
            prepMaybe (Just x) = Just $ prepare x


loadCovenant :: Maybe Covenant -> IO (Maybe Covenant)
loadCovenant Nothing  = trace "loadCovenant -> Nothing" $ return Nothing
loadCovenant (Just c)
    | isNothing st' = trace "loadCovenant -> Nothing to load" $ return $ Just c
    | isNothing fn = trace "loadCovenant -> Nothing to load" $ return $ Just c
    | otherwise = parseFromFile CSV.csvFile (fromJust fn) >>=
       ( \ lib -> return $ Just $ c { covenantState = Just $ st { library = g lib } } )
       where fn = librarycsv st
             st = fromJust st'
             st' = covenantState c
             g (Left _) = []
             g (Right x) = sort $ map fromCSVline x

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
