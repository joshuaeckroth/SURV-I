
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

-- | Assert detections
--
-- Hypothesize the detections then declare them as factual,
-- and requiring explanation.
assertDetections :: CategoryID       -- ^ Hypothesis category
                 -> WorldState       -- ^ World state
                 -> World WorldState -- ^ Resulting world
assertDetections catID ws = assertDetections' catID (getDetections (curFrame ws)) ws
    where
      assertDetections' _     []         ws = return ws
      assertDetections' catID (det:dets) ws =
          assertDetections' catID dets ws'
              where
                hypID            = nextHypID ws
                newCurrentDetIDs = [hypID] ++ (currentDetIDs ws)
                newHypIDs        = [hypID] ++ (hypIDs ws)
                newDetIDs        = (detIDs ws) ++ [hypID]
                newDetMap        = insert hypID det (detMap ws)
                newMind          = setFactual (fromList [hypID])
                                   (addHypothesis hypID catID (const Medium) (mind ws))
                ws'              = ws { mind = newMind, hypIDs = newHypIDs, currentDetIDs = newCurrentDetIDs,
                                        detMap = newDetMap, detIDs = newDetIDs }

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
                det          = getItemFromMap (detMap ws) did
                nids         = filter (\h -> (getItemFromMap (noiseMap ws) h) == did) (noiseIDs ws)
                tids         = filter (\h -> let (Track det' _ _ _) = (getItemFromMap (trackMap ws) h) in det' == det) (trackIDs ws)
                hs           = IDSet.fromList (tids ++ nids)
                cID          = read $ show $ nextConstrainer ws
                cIDs         = map HasInt [cID..(cID + (IDSet.size hs))] :: [ConstrainerID]
                constrainers = [(constrainer, IDSet.delete object hs, object)
                                | (constrainer, object) <- zip cIDs (IDSet.toList hs) :: [(ConstrainerID, ObjectID)]]
                               :: [(ConstrainerID, SubjectIDs, ObjectID)]
                ws'          = ws { mind = foldl (\m (c, ss, o) -> addConstrainer c ss oneOf o m) (mind ws) constrainers }
