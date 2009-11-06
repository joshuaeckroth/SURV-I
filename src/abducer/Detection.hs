
module Detection where
import Types
import Vocabulary
import Data.Maybe

mkDetections :: CameraDetections -> [Hypothesis Detection]
mkDetections (CameraDetections cdets) = mergeMultCameraDets $ map (mkDetection cdets) cdets

mkDetection :: [CameraDetection]
            -> CameraDetection
            -> Hypothesis Detection
mkDetection cdets cdet =
    let hypId     = mkDetHypId cdet
        det       = Detection
                    { detectionId        = hypId
                    , detectionLat       = cameraDetectionLat cdet
                    , detectionLon       = cameraDetectionLon cdet
                    , detectionStartTime = cameraDetectionStartTime cdet
                    , detectionEndTime   = cameraDetectionEndTime cdet
                    , detectionArea      = cameraDetectionArea cdet
                    , detectionCamera    = Just cdet
                    }
        aPriori   = mkDetectionScore cdets cdet
        explains  = []
        implies   = []
        conflicts = []
    in Hyp det hypId aPriori explains implies conflicts

mkDetectionScore :: [CameraDetection] -> CameraDetection -> (Level -> Level)
mkDetectionScore cdets cdet
    | area <  800.0  = (\s -> corroboration SlightlyLow)
    | area >= 1000.0 = (\s -> corroboration Low)
    | otherwise      = (\s -> corroboration VeryLow)
    where area = cameraDetectionArea cdet
          -- is this detection (maybe) seen in more than one camera?
          -- this is judged to be true if there is at least one detection very close by
          corroboration = if (1 < (length $ filter (nearby cdet) cdets)) then
                              increaseLevel
                          else id
          nearby cdet cdet' = 20.0 > (cameraDetDist cdet cdet')

{- ADD TIME FACTOR AND COMPARE ACROSS ALL EXISTING DETECTIONS NOT JUST NEW ONES -}

-- ^ Keep an abritrary hypothesized detection but then filter out all those that
--   are likely the same but detected in different cameras.
mergeMultCameraDets :: [Hypothesis Detection] -> [Hypothesis Detection]
mergeMultCameraDets [] = []
mergeMultCameraDets (hdet:hdets) = [hdet] ++ (mergeMultCameraDets $ filter (not . likelySameHypDet hdet) hdets)

-- ^ Check if two detections are likely the same (possibly across different cameras).
likelySameHypDet :: Hypothesis Detection -> Hypothesis Detection -> Bool
likelySameHypDet (Hyp {entity = det1}) (Hyp {entity = det2}) =
    (detDist det1 det2 < 50.0) && (detDelta det1 det2 < 3.0)