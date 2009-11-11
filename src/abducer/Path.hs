module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List ((\\), sortBy)
import Debug.Trace

mkPaths :: HypothesisMap Entity -> [Path] -> [Movement] -> [Hypothesis Path]
mkPaths entityMap paths movs =
    let -- extract detections of movements for efficiency
        extractDets movs = map (\mov@(Movement _ detStart detEnd _) ->
                                (mov, (getEntity entityMap detStart, getEntity entityMap detEnd))) movs
        movsAndDets      = extractDets movs
        movChains        = genMovChains movsAndDets
        existingPaths    = map (\(Path _ (NonEmpty pmovs)) -> extractDets $
                                map (\mref -> getEntity entityMap (movementRefMovId mref)) pmovs) paths
        newPaths         = map mkPath movChains
        extendedPaths    = map mkPath $ concat $ map (extendPaths movChains) existingPaths
    in newPaths ++ extendedPaths

mkPath :: [(Movement, (Detection, Detection))]
       -> Hypothesis Path
mkPath movs =
    let hypId     = mkPathHypId (map fst movs)
        movRefs   = map extractMovHypId (map fst movs)
        score     = mkPathScore movs
        path      = Path (Path_Attrs hypId (show $ score Medium) "") (NonEmpty $ map mkMovementRef movRefs)
        aPriori   = score
        explains  = movRefs
        implies   = movRefs
        conflicts = []
    in Hyp path hypId aPriori explains implies conflicts

extendPaths :: [[(Movement, (Detection, Detection))]]
            -> [(Movement, (Detection, Detection))]
            -> [[(Movement, (Detection, Detection))]]
extendPaths movChains pmovs =
    map (\movs -> pmovs ++ movs) $
        filter (\movs -> (movsTemporallyConnected (last pmovs) (head movs)) &&
                         (movsSpatiallyConnected (last pmovs) (head movs))) movChains

mkPathScore :: [(Movement, (Detection, Detection))] -> (Level -> Level)
mkPathScore movs
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 30.0 && duration > 5.0 = (\s -> High)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 40.0 && duration > 4.0 = (\s -> SlightlyHigh)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 50.0 && duration > 3.0 = (\s -> Medium)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 60.0 && duration > 2.0 = (\s -> SlightlyLow)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 70.0 && duration > 1.0 = (\s -> Low)
    | otherwise = (\s -> VeryLow)
    where avgChainDiffs = (sumChainDifferences movs) / (fromIntegral $ length movs)
          sumSpeedDiffs = sumChainSpeedDifferences movs
          duration      = let detStart = (fst . snd) $ last movs
                              detEnd   = (snd . snd) $ head movs
                          in detDelta detEnd detStart

sumChainSpeedDifferences :: [(Movement, (Detection, Detection))] -> Double
sumChainSpeedDifferences movs =
    let avgChainSpeed = (sum $ movChainSpeeds movs) / (fromIntegral $ length movs)
    in sum $ map (\s -> abs (s - avgChainSpeed)) $ movChainSpeeds movs

-- | Find the sum of differences (distance * time) of starting and ending
--   points of each link in a movement chain.
sumChainDifferences :: [(Movement, (Detection, Detection))] -> Double
sumChainDifferences []     = 0.0
sumChainDifferences (_:[]) = 0.0
sumChainDifferences (((_, (_, detEnd))):((_, (detStart, _))):movs) =
    (detDist detEnd detStart) * (detDelta detEnd detStart) + (sumChainDifferences movs)

-- | Find the speed each of link in the chain
movChainSpeeds :: [(Movement, (Detection, Detection))] -> [Double]
movChainSpeeds [] = []
movChainSpeeds ((_, (detStart, detEnd)):movs) =
    [(detDist detEnd detStart) / (detDelta detEnd detStart)] ++ (movChainSpeeds movs)

-- | Find movement sequences that are longer than two movements, whose
--   movements \"connect\" to each other, and whose changes in speed
--   are minimal.
-- 
-- \"Connection\" is defined as a closeness of the end point of
-- the earlier movement to the start point of the later movement.
-- This connection property is implemented in 'movsSpatiallyConnected'
-- and 'movsTemporallyConnected'; we do this successive filtering
-- for efficiency.
genMovChains :: [(Movement, (Detection, Detection))] -> [[(Movement, (Detection, Detection))]]
genMovChains movs = 
    let connectedMovs = filter (\movs -> length movs >= 4) $ nonEmptySubMovChains (sortBy movStartTimeCompare movs)
        -- movements that have a similar speed
        simSpeedMovs = filter (\movs -> sumChainSpeedDifferences movs < 100.0) connectedMovs
    in simSpeedMovs

nonEmptySubMovChains :: [(Movement, (Detection, Detection))]
                     -> [[(Movement, (Detection, Detection))]]
nonEmptySubMovChains [] = []
nonEmptySubMovChains (mov:movs) = [mov] : foldr f [] (nonEmptySubMovChains movs)
    where
      f [] r = [mov] : r
      f ys@(mov':movs') r = if (movsTemporallyConnected mov mov') && (movsSpatiallyConnected mov mov')
                            then ys : (mov : ys) : r
                            else ys : r

movStartTimeCompare :: (Movement, (Detection, Detection))
                    -> (Movement, (Detection, Detection))
                    -> Ordering
movStartTimeCompare (_, (detStart1, _)) (_, (detStart2, _)) =
    compare (detectionStartTime detStart1) (detectionStartTime detStart2)

-- | Determine if two movements are \"connected\".
-- 
-- Two movements are connected if the end point of the earlier movement
-- is with 0.0 units of the start point of the later movement.
movsSpatiallyConnected :: (Movement, (Detection, Detection))
                       -> (Movement, (Detection, Detection))
                       -> Bool
movsSpatiallyConnected (_, (detStart1, detEnd1)) (_, (detStart2, detEnd2)) =
    (detDist detEnd1 detStart2 == 0.0) && (detDistanceToSegment detStart1 detEnd1 detEnd2 < 30.0)

movsTemporallyConnected :: (Movement, (Detection, Detection))
                        -> (Movement, (Detection, Detection))
                        -> Bool
movsTemporallyConnected (_, (_, detEnd)) (_, (detStart, _)) =
    let delta = detDelta detStart detEnd
    in delta == 0.0

-- from http://www.gamedev.net/community/forums/viewreply.asp?ID=1250842
detDistanceToSegment :: Detection -> Detection -> Detection -> Double
detDistanceToSegment det1 det2 det3 =
    let dist (lat1, lon1) (lat2, lon2) = sqrt $ ((lat1 - lat2) ** 2.0) + ((lon1 - lon2) ** 2.0)
        lat1 = detectionLat det1
        lon1 = detectionLon det1
        lat2 = detectionLat det2
        lon2 = detectionLon det2
        lat3 = detectionLat det3
        lon3 = detectionLon det3
        det1to3 = (lat3 - lat1, lon3 - lon1)
        segLength = dist (lat1, lon1) (lat2, lon2)
        unitSeg = ((lat2 - lat1) / segLength, (lon2 - lon1) / segLength)
        intersectDist = (fst unitSeg) * (fst det1to3) + (snd unitSeg) * (snd det1to3)
        intersectPoint
            | intersectDist < 0.1 = (lat1, lon1)
            | intersectDist > segLength = (lat2, lon2)
            | otherwise = ((fst unitSeg) * intersectDist + lat1, (snd unitSeg) * intersectDist + lon1)
    in dist intersectPoint (lat3, lon3)
