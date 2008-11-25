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

-- | Execute the abduction
runAbducer :: [Frame]          -- ^ List of frames (which contain acquisitions)
           -> World WorldState -- ^ Existing world
           -> World WorldState -- ^ Resulting world
runAbducer frames world = world >>= runAbducer' frames
    where
      runAbducer' :: [Frame] -> WorldState -> World WorldState
      runAbducer' []             ws = return ws
      runAbducer' (f:fs) ws =
          do
            let
                catID = HasInt 0 :: CategoryID

                -- insert frame, reset 'current' acquisition, noise, track hypotheses
                ws'   = ws { frame = f, acqIDs = [], noiseIDs = [], trackIDs = [] } 

            recordFrame ws' >>=
                        hypothesizeAcquisitions catID >>=
                        hypothesizeNoise catID >>=
                        hypothesizeTracks catID >>=
                     -- hypothesizeClassifications catID >>=
                        (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) High (mind ws'')) } ) >>=
                        recordAcquisitions >>=
                        updateNoise >>=
                        recordNoise >>=
                        updateTracks >>=
                        recordTracks >>=
                        runAbducer' fs

