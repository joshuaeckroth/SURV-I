
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
        score     = mkDetectionScore cdets cdet
        det       = Detection
                    { detectionId        = hypId
                    , detectionLat       = cameraDetectionLat cdet
                    , detectionLon       = cameraDetectionLon cdet
                    , detectionStartTime = cameraDetectionStartTime cdet
                    , detectionEndTime   = cameraDetectionEndTime cdet
                    , detectionArea      = cameraDetectionArea cdet
                    , detectionScore     = show (score Medium)
                    , detectionCamera    = Just cdet
                    }
        aPriori   = score
        explains  = []
        implies   = []
        conflicts = []
    in Hyp det hypId aPriori explains implies conflicts

mkDetectionScore :: [CameraDetection] -> CameraDetection -> (Level -> Level)
mkDetectionScore cdets cdet
    | area <  800.0  = (\s -> corroboration Medium)
    | area >= 1000.0 = (\s -> corroboration SlightlyLow)
    | otherwise      = (\s -> corroboration Low)
    where area = cameraDetectionArea cdet
          -- is this detection (maybe) seen in more than one camera?
          -- this is judged to be true if there is at least one detection very close by
          corroboration = if (1 < (length $ filter (nearby cdet) cdets)) then
                              increaseLevelBy 2
                          else id
          nearby cdet cdet' = 20.0 > (cameraDetDist cdet cdet')

-- ^ Keep an abritrary hypothesized detection but then filter out all those that
--   are likely the same but detected in different cameras.
mergeMultCameraDets :: [Hypothesis Detection] -> [Hypothesis Detection]
mergeMultCameraDets [] = []
mergeMultCameraDets (hdet@(Hyp {entity = det}):hdets) =
    [hdet] ++ (mergeMultCameraDets $ filter (\(Hyp {entity = det'}) -> not $ likelySameDet det det') hdets)

-- ^ Check if two detections are likely the same (possibly across different cameras).
likelySameDet :: Detection -> Detection -> Bool
likelySameDet det1 det2 = (detDist det1 det2 < 30.0) && (detDelta det1 det2 < 1.0)
