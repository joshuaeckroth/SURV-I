module Movement
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkMovements :: [Detection] -> [Hypothesis Movement]
mkMovements dets =
    let closeDetPairs = [(det1, det2, dist, delta, areaDiff, speed) |
                         det1 <- dets, det2 <- dets,
                         let dist     = detDist det1 det2,
                         let delta    = detDelta det1 det2,
                         let areaDiff = detAreaDiff det1 det2,
                         let speed    = detSpeed det1 det2,
                         det1 /= det2,
                         dist < 150.0,
                         areaDiff < 2000.0,
                         speed < 100.0,
                         delta < 1.5, delta > 0.0,
                         detBefore det1 det2]
    in map (mkMovement closeDetPairs) closeDetPairs

mkMovement :: [(Detection, Detection, Double, Time, Double, Double)]
           -> (Detection, Detection, Double, Time, Double, Double)
           -> Hypothesis Movement
mkMovement dets (det1, det2, dist, delta, areaDiff, speed) =
    let hypId     = mkMovHypId [det1, det2]
        detHypId1 = extractDetHypId det1
        detHypId2 = extractDetHypId det2
        score     = mkMovementScore dets (det1, det2, dist, delta, areaDiff, speed)
        mov       = Movement hypId detHypId1 detHypId2 (show $ score Medium)
        aPriori   = score
        explains  = [detHypId1, detHypId2]
        implies   = [detHypId1, detHypId2]
        conflicts = []
    in Hyp mov hypId aPriori explains implies conflicts

mkMovementScore :: [(Detection, Detection, Double, Time, Double, Double)]
                -> (Detection, Detection, Double, Time, Double, Double)
                -> (Level -> Level)
mkMovementScore dets (det1, det2, dist, delta, areaDiff, speed)
    | dist < 50.0 && delta < 0.5  = (\s -> corroboration Low)
    | dist < 100.0 && delta < 1.0 = (\s -> corroboration SlightlyLow)
    | otherwise                   = (\s -> corroboration VeryLow)
    -- corroboration depends on two camera detections appearing similar (so far, just area)
    where corroboration = if areaDiff < 200.0 then increaseLevel else id
