
module Detection where
import Types
import World
import Frame
import Reasoner.Core
import Reasoner.Types
import Reasoner.Constrainers
import Vocabulary
import WrappedInts.Types (HasInt(..))
import WrappedInts.IDSet (fromList)
import WrappedInts.IDMap (insert, getItemFromMap, keys)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import Text.XML.HaXml.Types as HaXml
import Data.List ((\\))

-- | Hypothesize detections
hypothesizeDetections :: CategoryID       -- ^ Hypothesis category
                      -> WorldState       -- ^ World state
                      -> World WorldState -- ^ Resulting world
hypothesizeDetections catID ws = hypothesizeDetections' catID (getDetections (curFrame ws)) ws
    where
      hypothesizeDetections' _     []         ws = return ws
      hypothesizeDetections' catID (det:dets) ws = hypothesizeDetections' catID dets ws'
              where
                hypID            = nextHypID ws
                newCurrentDetIDs = [hypID] ++ (currentDetIDs ws)
                newHypIDs        = [hypID] ++ (hypIDs ws)
                newDetIDs        = (detIDs ws) ++ [hypID]
                newDetMap        = insert hypID det (detMap ws)
                newMind          = addHypothesis hypID catID (scoreDetection (curFrame ws) hypID newDetMap) (mind ws)
                ws'              = ws { mind = newMind, hypIDs = newHypIDs, currentDetIDs = newCurrentDetIDs,
                                        detMap = newDetMap, detIDs = newDetIDs }

-- | Score a detection hypothesis (this is the hypothesis's a priori score)
scoreDetection :: Frame        -- ^ Current frame
               -> DetectionID  -- ^ Detection ID to score
               -> DetectionMap -- ^ Current detection map
               -> Level        -- ^ \'Current situation\'
               -> Level        -- ^ Resulting score
scoreDetection frame did dm l
    | area < 30.0 = Highest
    | otherwise   = Lowest
    where
      det   = getItemFromMap dm did
      area  = detProp detArea det

-- | Human format of detections log
showDetections :: [DetectionID] -- ^ Detection IDs to log
               -> DetectionMap  -- ^ Current detection map
               -> Frame         -- ^ Current frame
               -> [String]      -- ^ Human log
showDetections []         _  _     = []
showDetections (did:dids) dm frame =
    ["Detection " ++ (show did) ++ " [" ++
     "camera: " ++ (detProp detCamera det) ++ ", " ++
     "cx: " ++ (show $ detProp detCx det) ++ ", " ++
     "cy: " ++ (show $ detProp detCy det) ++ ", " ++
     "area: " ++ (show $ detProp detArea det) ++ ", " ++
     "frameNumber: " ++ (show framenumber) ++ ", " ++
     "frameTime: " ++ (show frametime) ++ ", " ++
     "age: " ++ (show $ curframetime - frametime) ++ "s]"]
    ++ showDetections dids dm frame
    where
      det          = getItemFromMap dm did
      framenumber  = frameProp frameNumber $ detProp detFrame det
      frametime    = frameProp frameTime $ detProp detFrame det
      curframetime = frameProp frameTime frame

detectionsToXml :: [DetectionID] -> DetectionMap -> [HaXml.Content ()]
detectionsToXml []     _     = []
detectionsToXml (did:dids) m =
    [worldElem "Detection" [("id", show did),
                            ("camera", detProp detCamera det),
                            ("area", show $ detProp detArea det),
                            ("cx", show $ detProp detCx det),
                            ("cy", show $ detProp detCy det)] []]
    ++ detectionsToXml dids m
    where
      det = getItemFromMap m did

recordDetections :: WorldState -> World WorldState
recordDetections ws =
    recordWorldEvent (showDetections dids dm frame, recordWorldEventInFrame frame $ detectionsToXml dids dm) >>
    return ws
    where
      dids   = currentDetIDs ws -- only output current detections
      dm     = detMap ws
      frame  = curFrame ws

-- | Keep only detections that are younger than a few seconds, and erase current detection list
updateDetections :: WorldState       -- ^ World state
                 -> World WorldState -- ^ Resulting world
updateDetections ws =
    recordWorldEvent (["Removed old detections:"] ++ (showDetections oldDetIDs (detMap ws) (curFrame ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, detIDs = newDetIDs, currentDetIDs = [] })
    where
      frametime = frameProp frameTime (curFrame ws)
      oldDetIDs = filter (\h -> 2.0 < (frametime - (frameProp frameTime $ detProp detFrame $ (getItemFromMap (detMap ws) h)))) (detIDs ws)
      newDetIDs = (detIDs ws) \\ oldDetIDs
      newMind   = foldl (\m h -> removeHypothesis h m) (mind ws) oldDetIDs

-- | Add constraints for all hypotheses explaining the same detection
--
-- Check noise and track hypotheses. The constraint is a 'Reasoner.Constraints.oneOf' constraint
-- which allows keeping only one of the hypotheses per detection.
constrainDetectionExplainers :: WorldState       -- ^ World state
                             -> World WorldState -- ^ Resulting world
constrainDetectionExplainers ws = constrainDetectionExplainers' (detIDs ws) ws
    where
      constrainDetectionExplainers' :: [DetectionID] -> WorldState -> World WorldState
      constrainDetectionExplainers' []         ws = return ws
      constrainDetectionExplainers' (did:dids) ws =
          recordWorldEvent ( [""] {-- ["Constrained detection " ++ (show did) ++ ":"] ++ (map show (getConstrainers (mind ws'))) --}, emptyElem) >>
          constrainDetectionExplainers' dids ws'
              where
                det  = getItemFromMap (detMap ws) did
                nids = filter (\h -> (getItemFromMap (noiseMap ws) h) == did) (noiseIDs ws)
                tids = filter (\h -> let (Track det' _ _ _) = (getItemFromMap (trackMap ws) h) in det' == det) (trackIDs ws)
                ws'  = addCyclicConstrainers (tids ++ nids) constrainerOneOf ws

lookupDetectionID :: Detection -> DetectionMap -> DetectionID
lookupDetectionID det dm = IDMap.foldWithKey (\k e d -> if e == det then k else d) 0 dm
