module Abducer where
import Types
import World
import Frame
import Detection
import Noise
import Track
import Vocabulary
import Reasoner.Types
import Reasoner.Core
import WrappedInts.Types (HasInt(..))
import WrappedInts.IDSet (toList)

-- | Execute the abduction
runAbducer :: Frame            -- ^ Frame of detections
           -> WorldState       -- ^ Existing world
           -> World WorldState -- ^ Resulting world
runAbducer frame ws = world
    where
      catID = HasInt 0 :: CategoryID
      world = ((return $ cleanWorld frame ws) >>=
               recordFrame >>=
                  
               assertDetections catID >>=
               hypothesizeNoise catID >>=
               hypothesizeTracks catID >>=
               hypothesizeSplitTracks catID >>=
               -- hypothesizeClassifications catID >>=
               constrainDetectionExplainers >>=
                  
               (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) Medium (mind ws'')) }) >>=

               updateNoise >>=
               updateTracks >>=
                  
               recordNoise >>=
               recordTracks >>=
               recordDetections)

