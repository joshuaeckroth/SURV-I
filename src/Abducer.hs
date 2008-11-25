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
          ws'   = ws { acqIDs = [], noiseIDs = [], trackIDs = [] } -- reset 'current' acquisition, noise, track hypotheses

      recordFrame frame ws' >>=
                  hypothesizeAcquisitions catID frame (getAcquisitions frame) >>=
                  hypothesizeNoise catID >>=
                  hypothesizeTracks catID >>=
               -- hypothesizeClassifications catID >>=
                  (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) High (mind ws'')) } ) >>=
                  recordAcquisitions >>=
                  updateNoise >>=
                  recordNoise >>=
                  updateTracks >>=
                  recordTracks >>=
                  runAbducer' frames

