---------------------------------------------------------------------------- -- |
-- Module      :  ArM.IO
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
-- Maintainer  :  hg+gamer@schaathun.net
-- Description :  
--    High-level functions to parse ArMChar files and generate markdown sheets.
--
-- |
-- Some objects, like the Spell Database, are stored in CSV, others
-- in JSON or YAML, including Character and Covenant.
--
-- The submodules `ArM.IO.*` provide technical documentation for the
-- constituent functions.  
module ArM.IO (readSaga,writeSaga) where

import ArM.IO.Read
import ArM.IO.Write

import ArM.Markdown
import ArM.Advancement
import ArM.Types.Saga
import ArM.Helper

import Data.OList

-- | Load the saga and all its constituent objects from the given file.
--
-- This calls both `readSagaFile` and `loadSaga`.
-- Characters and covenants are loaded and advanced.
-- The `Saga` object contains a list of characters and one of convenants.
readSaga :: String -- ^ Filename
         -> IO (Maybe Saga)
readSaga fn = readSagaFile fn >>= passMaybe loadSaga

-- |
-- Write markdown files for the saga and all its covenants and characters.
writeSaga :: Saga -> IO ()
writeSaga saga = do
      writeOList (rootDir saga ++ "/index.md") $ printMD saga

      let sagas = advanceSaga saga

      writeSagaStates sagas
      writeSagaAnnals'  sagas
      return () 
  where
     writeSagaAnnals' [] = error "Empty list of sagas" 
     writeSagaAnnals' (x:_) = writeSagaAnnals x

