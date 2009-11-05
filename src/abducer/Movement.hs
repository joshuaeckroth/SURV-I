module Movement
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkMovements :: [Hypothesis Detection] -> [Hypothesis Movement]
mkMovements hDets =
    let closeDetPairs = [(hdet1, hdet2, dist, delta) |
                         hdet1@(Hyp { entity = det1 }) <- hDets,
                         hdet2@(Hyp { entity = det2 }) <- hDets,
                         let dist  = detDist det1 det2,
                         let delta = detDelta det1 det2,
                         det1 /= det2,
                         dist < 70.0, delta < 6.0, detBefore det1 det2]
    in map (mkMovement closeDetPairs) closeDetPairs

mkMovement :: [(Hypothesis Detection, Hypothesis Detection, Double, Time)]
           -> (Hypothesis Detection, Hypothesis Detection, Double, Time)
           -> Hypothesis Movement
mkMovement hdets (hdet1@(Hyp { entity = det1 }),
                  hdet2@(Hyp { entity = det2 }), dist, delta) =
    let hypId     = mkMovHypId [det1, det2]
        mov       = Movement (Movement_Attrs hypId) (NonEmpty [det1, det2])
        aPriori   = mkMovementScore hdets (hdet1, hdet2, dist, delta)
        explains  = [hdet1, hdet2]
        implies   = [hdet1, hdet2]
        conflicts = [] :: [Hypothesis Detection] {- arbitrary type signature -}
    in Hyp mov hypId aPriori explains implies conflicts

mkMovementScore :: [(Hypothesis Detection, Hypothesis Detection, Double, Time)]
                -> (Hypothesis Detection, Hypothesis Detection, Double, Time)
                -> (Level -> Level)
mkMovementScore hdets (hdet1@(Hyp { entity = det1 }),
                       hdet2@(Hyp { entity = det2 }), dist, delta)
    | dist < 30.0 && delta < 2.5 = (\s -> corroboration Low)
    | dist < 50.0 && delta < 4.5 = (\s -> corroboration SlightlyLow)
    | otherwise                  = (\s -> corroboration VeryLow)
    -- corroboration depends on two camera detections appearing similar (so far, just area)
    where corroboration =
              let (Just cdet1) = detectionCamera det1
                  (Just cdet2) = detectionCamera det2
                  areaDiff = abs $ (cameraDetectionArea cdet1) -
                             (cameraDetectionArea cdet2)
              in if areaDiff < 500 then increaseLevel else id

