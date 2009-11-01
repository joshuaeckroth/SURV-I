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
import qualified WrappedInts.IDMap as IDMap
import Data.List ((\\))

-- | Execute the abduction
runAbducer :: Frame            -- ^ Frame of detections
           -> WorldState       -- ^ Existing world
           -> World WorldState -- ^ Resulting world
runAbducer frame ws = world
    where
      catID = HasInt 0 :: CategoryID
      world = ((return $ cleanWorld frame ws) >>=
               updateDetections >>=
               recordFrame >>=

               hypothesizeDetections catID >>=
               hypothesizeTracks catID >>=
               hypothesizeSplitTracks catID >>=
               -- constrainDetectionExplainers >>=

               (\ws -> (recordWorldEvent (["Before reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (showConstrainers ws)
                                          ++ ["Tracks:"] ++ (showTracks (IDMap.keys (trackMap ws)) (trackMap ws) (detMap ws) (curFrame ws))
                                         , emptyElem)
                        >> return ws)) >>=
                  
               (\ws -> return ws { mind = (reason (ReasonerSettings False) High (mind ws)) }) >>=

               (\ws -> (recordWorldEvent (["After reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (map show $ getConstrainers $ mind ws)
                                          ++ ["Adjusters:"] ++ (map show $ getAdjusters $ mind ws)
                                         , emptyElem)
                        >> return ws)) >>=
                  
               updateTracks >>=
               recordTracks)
