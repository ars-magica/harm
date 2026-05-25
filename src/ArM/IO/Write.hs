-- |
-- Module      :  ArM.IO.Write
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-- Description :  Functions to write character and covenant sheets.
--
-- This module is an internal module used by `ArM.IO`.
--
-- The functions in this module generally take a filename (String)
-- and returns a Maybe object by attempting to parse the relevant 
-- file.

module ArM.IO.Write where

import System.Directory

import ArM.Markdown
import ArM.Sheet
import ArM.Types.Harm
import ArM.Story
import ArM.Saga

import Data.OList

-- | Write charactersheets in MarkDown
-- File name is derived from the character name.
writeObjects :: (HarmObject h, HOutput h) 
             => String  -- ^ Directory for the output files
             -> Saga    -- ^ Saga whose objects are written
             -> [ h ]   -- ^ List of objects to write
             -> IO ()
writeObjects dir saga cs = mapM wf  cs >> return ()
         where wf c = (writeOList (fn c) $ printSheetMD saga c)
               fn c = dir ++ "/" ++ stateName c ++ ".md"

-- | Write the sheets for the current season. 
writeSagaState :: Saga -> IO ()
writeSagaState saga = 
   createDirectoryIfMissing True dir >>
   writeOList (dir ++ "index.md") (sagaStateMD saga) >>
   writeObjects dir saga (characterList saga) >>
   writeObjects dir saga (covenantList saga) >>
   writeObjects dir saga (map getLibrary $ covenantList saga)
       where dir = rootDir saga ++ fn ++ "/"
             fn = showKey saga

-- | Write the sheets for each season recorded. 
writeSagaStates :: [Saga] -> IO ()
writeSagaStates [] = return ()
writeSagaStates (x:xs) = writeSagaState x >> writeSagaStates xs

-- | Write the annals, which summarise the events per season.
writeSagaAnnals :: Saga -> IO ()
writeSagaAnnals saga = writeOList fn $ ann saga
    where fn = rootDir saga ++ "/0001_Annals.md"
          ann = OList . (OString "# Annals":) . map printMD . sagaAnnals 


