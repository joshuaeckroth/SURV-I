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

               assertDetections catID >>=
               hypothesizeNoise catID >>=
               hypothesizeTracks catID >>=
               hypothesizeSplitTracks catID >>=
               constrainDetectionExplainers >>=

               {--
               (\ws -> (recordWorldEvent (["Before reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (map show $ getConstrainers $ mind ws)
                                          ++ ["Explainers:"] ++ (map show $ getExplainers $ mind ws)
                                          ++ ["Hypotheses:"] ++ (map (\h -> unlines $ showHypothesis h (mind ws)) ((detIDs ws) ++ (noiseIDs ws) ++ (trackIDs ws)))
                                         , emptyElem)
                        >> return ws)) >>=
               --}
                  
               (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) Medium (mind ws'')) }) >>=

               {--
               (\ws -> (recordWorldEvent (["After reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (map show $ getConstrainers $ mind ws)
                                          ++ ["Explainers:"] ++ (map show $ getExplainers $ mind ws)
                                          ++ ["Hypotheses:"] ++ (map (\h -> unlines $ showHypothesis h (mind ws)) ((detIDs ws) ++ (noiseIDs ws) ++ (trackIDs ws)))
                                         , emptyElem)
                        >> return ws)) >>=
               --}
                  
               updateNoise >>=
               updateTracks >>=

               recordNoise >>=
               recordTracks >>=
               recordDetections)
