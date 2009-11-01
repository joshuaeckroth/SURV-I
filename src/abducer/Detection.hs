
module Detection where
import Types
import Vocabulary

mkDetections :: CameraDetections -> [Hypothesis Detection]
mkDetections (CameraDetections cdets) = map (mkDetection cdets) cdets

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
                    }
        aPriori   = mkDetectionScore cdets cdet
        explains  = [] :: [Hypothesis Detection] {- these type signatures are arbitrary but needed -}
        depends   = [] :: [Hypothesis Detection]
        conflicts = [] :: [Hypothesis Detection]
    in Hyp det hypId aPriori explains depends conflicts

mkDetectionScore :: [CameraDetection] -> CameraDetection -> (Level -> Level)
mkDetectionScore cdets cdet
    | area < 50.0  = (\s -> corroboration Low)
    | area > 150.0 = (\s -> corroboration High)
    | otherwise    = (\s -> corroboration Medium)
    where area = cameraDetectionArea cdet
          -- is this detection (maybe) seen in more than one camera?
          corroboration = if (1 < (length $ filter (nearby cdet) cdets)) then
                              increaseLevel
                          else id
          nearby cdet cdet' = 15.0 > (cameraDetDist cdet cdet')

{- ADD TIME FACTOR AND COMPARE ACROSS ALL EXISTING DETECTIONS NOT JUST NEW ONES -}