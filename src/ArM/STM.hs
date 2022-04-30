module ArM.STM where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Swish.RDF.Graph as G
import Data.Maybe (fromJust)
import Network.URI (URI)

import qualified ArM.CharacterMap as CM
import qualified ArM.Character as C
import qualified ArM.Types.Character as TC
import qualified ArM.Resources as AR

import ArM.Rules.Aux
import ArM.Resources
import Swish.RDF.Graph



data MapState = MapState { graph :: G.RDFGraph,
                           schemaGraph :: G.RDFGraph,
                           resourceGraph :: G.RDFGraph }

getSTM res schema g char = STM.newTVarIO MapState { graph = g, schemaGraph = schema, resourceGraph = res  }

getStateGraph :: STM.TVar MapState -> IO G.RDFGraph
getStateGraph st = fmap graph $ STM.readTVarIO st
getSchemaGraph :: STM.TVar MapState -> IO G.RDFGraph
getSchemaGraph st = fmap schemaGraph $ STM.readTVarIO st
getResourceGraph :: STM.TVar MapState -> IO G.RDFGraph
getResourceGraph st = fmap resourceGraph $ STM.readTVarIO st

        
persistRule = makeCRule "persistRule" 
    [ arc sVar pVar cVar,
      arc pVar typeRes armPersistentProperty ]
    [ arc sVar pVar cVar ]

lookup :: STM.TVar MapState -> String -> String -> Int 
       -> IO (Maybe CM.CharacterRecord)
lookup stateVar char season year = do
          st <- STM.readTVarIO stateVar
          let g = graph st
          let res = resourceGraph st
          print $ char ++ " - " ++ season ++ " - " ++ show year
          let cl =  C.getAllCS g $ AR.armcharRes char
          print $  AR.armcharRes char
          let charstring = "armchar:" ++ char
          case (cl) of
             Nothing -> return Nothing
             (Just x) -> return $ CM.lookup cmap charstring season year
                where  cmap = CM.insertListS res CM.empty $ x

getResource :: G.RDFGraph -> G.RDFLabel -> Maybe G.RDFGraph
getResource g label = Nothing


putGraph :: G.RDFGraph -> G.RDFGraph -> G.RDFGraph -> G.RDFGraph
putGraph g g0 g1 = G.merge (G.delete g0 g) g1

-- | Update the state graph with the given Advancement object.
putAdvancement :: STM.TVar MapState -> TC.Advancement -> IO G.RDFGraph
putAdvancement stateVar adv = do 
         STM.atomically $ do
             st <- STM.readTVar stateVar
             let g = graph st
             let adv0 = TC.fromRDFGraph g label :: TC.Advancement
             let g0 = TC.makeRDFGraph adv0
             let gg = putGraph g g0 g1
             STM.writeTVar stateVar $ st { graph = gg }
             return gg
          where g1 = fwdApplySimple persistRule $ TC.makeRDFGraph adv
                   -- Turn the new object into a graph
                label = TC.rdfid adv
-- g1 has to be pruned for derived properties
-- Check that arm:advanceCharacter is retained in output
-- Check for conflicting advancements 

putResource :: STM.TVar MapState -> G.RDFGraph -> G.RDFGraph
             -> IO G.RDFGraph
putResource stateVar oldres newres =  do
      STM.atomically $ do
          st <- STM.readTVar stateVar
          let g = graph st
          STM.writeTVar stateVar $ st { graph = newres }
          return $ newres


-- postResource :: STM.TVar MapState -> G.RDFLabel -> G.RDFGraph
             -- -> IO (Maybe G.RDFGraph)
-- postResource stateVar label newres = do 
      -- STM.atomically $ do
          -- st <- STM.readTVar stateVar
          -- let g = graph st
          -- case (putGraph g label newres) of
             -- Nothing -> return Nothing
             -- (Just gg) -> do
                          -- STM.writeTVar stateVar $ st { graph = gg }
                          -- return $ Just gg
