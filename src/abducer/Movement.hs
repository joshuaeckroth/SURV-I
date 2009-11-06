module Movement
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkMovements :: [Detection] -> [Hypothesis Movement]
mkMovements dets =
    let closeDetPairs = [(det1, det2, dist, delta) |
                         det1 <- dets, det2 <- dets,
                         let dist  = detDist det1 det2,
                         let delta = detDelta det1 det2,
                         det1 /= det2,
                         dist < 75.0, delta < 1.5, detBefore det1 det2]
    in map (mkMovement closeDetPairs) closeDetPairs

mkMovement :: [(Detection, Detection, Double, Time)]
           -> (Detection, Detection, Double, Time)
           -> Hypothesis Movement
mkMovement dets (det1, det2, dist, delta) =
    let hypId     = mkMovHypId [det1, det2]
        mov       = Movement (Movement_Attrs hypId) (NonEmpty [det1, det2])
        aPriori   = mkMovementScore dets (det1, det2, dist, delta)
        explains  = [detectionId det1, detectionId det2]
        implies   = [detectionId det1, detectionId det2]
        conflicts = []
    in Hyp mov hypId aPriori explains implies conflicts

mkMovementScore :: [(Detection, Detection, Double, Time)]
                -> (Detection, Detection, Double, Time)
                -> (Level -> Level)
mkMovementScore dets (det1, det2, dist, delta)
    | dist < 30.0 && delta < 0.5 = (\s -> corroboration Low)
    | dist < 60.0 && delta < 1.0 = (\s -> corroboration SlightlyLow)
    | otherwise                  = (\s -> corroboration VeryLow)
    -- corroboration depends on two camera detections appearing similar (so far, just area)
    where corroboration =
              let (Just cdet1) = detectionCamera det1
                  (Just cdet2) = detectionCamera det2
                  areaDiff = abs $ (cameraDetectionArea cdet1) -
                             (cameraDetectionArea cdet2)
              in if areaDiff < 200 then increaseLevel else id

