
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
                hypID      = nextHypID ws
                newHypIDs  = [hypID] ++ (hypIDs ws)
                newDetIDs  = (detIDs ws) ++ [hypID]
                newDetMap  = insert hypID det (detMap ws)
                newMind    = setFactual (fromList [hypID])
                             (addHypothesis hypID catID (const Medium) (mind ws))
                ws'        = ws { mind = newMind, hypIDs = newHypIDs, detMap = newDetMap, detIDs = newDetIDs }

detectionsToXml :: [DetectionID] -> DetectionMap -> [HaXml.Content ()]
detectionsToXml []     _     = []
detectionsToXml (did:dids) m =
    [worldElem "Detection" [("id", show did),
                            ("camera", frameProp frameCamera $ detProp detFrame det),
                            ("area", show $ detProp detArea det),
                            ("cx", show $ detProp detCx det),
                            ("cy", show $ detProp detCy det)] []]
    ++ detectionsToXml dids m
    where
      det = getItemFromMap m did

recordDetections :: WorldState -> World WorldState
recordDetections ws =
    recordWorldEvent ([""], recordWorldEventInFrame frame $ detectionsToXml dids m) >>
    return ws
    where
      dids   = detIDs ws
      m      = detMap ws
      frame  = curFrame ws

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
          recordWorldEvent (["Constrained detection " ++ (show did) ++ ":"] ++ (map show (getConstrainers (mind ws'))), emptyElem) >>
          constrainDetectionExplainers' dids ws'
              where
                det          = getItemFromMap (detMap ws) did
                nids         = keys (IDMap.filter ((==) did) (noiseMap ws))
                tids         = keys (IDMap.filter (\(Track det' _ _ _) -> det' == det) (trackMap ws))
                hs           = IDSet.fromList (tids ++ nids)
                cID          = read $ show $ nextConstrainer ws
                cIDs         = map HasInt [cID..(cID + (IDSet.size hs))] :: [ConstrainerID]
                constrainers = [(constrainer, IDSet.delete object hs, object)
                                | (constrainer, object) <- zip cIDs (IDSet.toList hs) :: [(ConstrainerID, ObjectID)]]
                               :: [(ConstrainerID, SubjectIDs, ObjectID)]
                ws'          = ws { mind = foldl (\m (c, ss, o) -> addConstrainer c ss oneOf o m) (mind ws) constrainers }
