module Abducer where
import Types
import World
import Frame
import Acquisition
import Noise
import Track
import Vocabulary
import Reasoner.Types
import Reasoner.Core
import WrappedInts.Types (HasInt(..))

runAbducer :: [Frame] -> World WorldState -> World WorldState
runAbducer frames world = world >>= runAbducer' frames

runAbducer' :: [Frame] -> WorldState -> World WorldState
runAbducer' []             ws = return ws
runAbducer' (frame:frames) ws =
    do
      let catID = HasInt 0 :: CategoryID
          ws'   = ws { acqIDs = [] } -- reset 'current' acquisition set

      recordFrame frame ws' >>=
                  hypothesizeAcquisitions catID frame (getAcquisitions frame) >>=
                  hypothesizeNoise catID >>=
                  hypothesizeTracks catID >>=
               -- hypothesizeClassifications catID >>=
                  (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) High (mind ws'')) } ) >>=
               -- outputHuman >>=
               -- outputXML >>=
                  runAbducer' frames


-- processFrames []                          _     = "End of frame\n\n"
-- processFrames ((Frame attrs acqs):frames) world =
--     let
--         catID  = HasInt 0 :: CategoryID
--         world' = processAcquisitions acqs catID world
--         in 
--           "World for frame " ++ show (frameNumber attrs) ++ "\n" ++
--                                  (showWorld world') ++ "\n\n" ++
--                                  processFrames frames world'
--
--                            unlines (showMind $ reason (ReasonerSettings False) High newMind) ++ "\n" ++
--                            "Tracks:\n" ++ (showTracks newTracks) ++ "\n" ++
--                            "New acquisitions:\n" ++ (showAcquisitions acqIDs newAcqMap) ++ "\n" ++
--                            processFrames frames newAcqMap newTracks newHs newMind
