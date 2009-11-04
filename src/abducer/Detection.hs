
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
        explains  = [] :: [Hypothesis Detection] {- these type signatures are arbitrary but needed -}
        depends   = [] :: [Hypothesis Detection]
        conflicts = [] :: [Hypothesis Detection]
    in Hyp det hypId aPriori explains depends conflicts

mkDetectionScore :: [CameraDetection] -> CameraDetection -> (Level -> Level)
mkDetectionScore cdets cdet
    | area <  300.0 = (\s -> corroboration Low)
    | area >= 600.0 = (\s -> corroboration High)
    | otherwise     = (\s -> corroboration Medium)
    where area = cameraDetectionArea cdet
          -- is this detection (maybe) seen in more than one camera?
          -- this is judged to be true if there is at least one detection very close by
          corroboration = if (1 < (length $ filter (nearby cdet) cdets)) then
                              increaseLevel
                          else id
          nearby cdet cdet' = 30.0 > (cameraDetDist cdet cdet')

{- ADD TIME FACTOR AND COMPARE ACROSS ALL EXISTING DETECTIONS NOT JUST NEW ONES -}

-- ^ Keep an abritrary hypothesized detection but then filter out all those that
--   are likely the same but detected in different cameras.
mergeMultCameraDets :: [Hypothesis Detection] -> [Hypothesis Detection]
mergeMultCameraDets [] = []
mergeMultCameraDets (hdet:hdets) = [hdet] ++ (mergeMultCameraDets $ filter (not . likelySameHypDet hdet) hdets)

-- ^ Check if two detections are likely the same (possibly across different cameras).
likelySameHypDet :: Hypothesis Detection -> Hypothesis Detection -> Bool
likelySameHypDet (Hyp {entity = det1}) (Hyp {entity = det2}) =
    (detDist det1 det2 < 30.0) && (detDelta det1 det2 < 1.0)