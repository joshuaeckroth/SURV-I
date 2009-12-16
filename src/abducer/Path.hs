module Path
where

import Types
import Context
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import qualified WrappedInts.IDSet as IDSet (fromList)
import World (getEntity, gatherEntities)
import Data.List ((\\), sortBy, nub,intercalate)
import Debug.Trace

mkPaths :: HypothesisMap Entity -> [Path] -> [Movement] -> [Hypothesis Path]
mkPaths entityMap paths movs =
    let -- extract detections of movements for efficiency
        extractDets movs     = map (\mov@(Movement _ detStart detEnd _) ->
                                    (mov, (getEntity entityMap detStart, getEntity entityMap detEnd))) movs
        movsAndDets          = extractDets movs
        movChains            = genMovChains movsAndDets
        existingPaths        = map (\(Path _ (NonEmpty pmovs)) -> extractDets $
                                    map (\mref -> getEntity entityMap (movementRefMovId mref)) pmovs) paths

        newPaths             = map mkPath $ filter (\(_, score) -> (score Medium) /= Low) $
                               map (\chain -> (chain, mkPathScore chain)) movChains

        extendedPaths        = map mkPath $ filter (\(_, score) -> (score Medium) /= Low) $
                               map (\chain -> (chain, mkPathScore chain)) $
                               nub $ concat $ map (extendPaths movChains) existingPaths

    in nub $ newPaths ++ extendedPaths

mkPath :: ([(Movement, (Detection, Detection))], (Level -> Level))
       -> Hypothesis Path
mkPath (movs, score) =
    let hypId     = mkPathHypId (map fst movs)
        movRefs   = map extractMovHypId (map fst movs)
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
    map (\movs -> nub $ pmovs ++ movs) $
    filter (\movs -> movsConnected (last pmovs) (head movs)) movChains

mkPathScore :: [(Movement, (Detection, Detection))] -> (Level -> Level)
mkPathScore movs
    | maxAngleDiff < 60.0 && (duration > 10.0)                        = (\s -> Highest)
    | maxAngleDiff < 60.0 && (duration > 7.0 && sumSpeedDiffs < 15.0) = (\s -> VeryHigh)
    | maxAngleDiff < 60.0 && (duration > 5.0 || sumSpeedDiffs < 20.0) = (\s -> High)
    | maxAngleDiff < 60.0 && (duration > 4.0 || sumSpeedDiffs < 50.0) = (\s -> SlightlyHigh)
    | otherwise = (\s -> Low)
    where sumSpeedDiffs = sumChainSpeedDifferences movs
          maxAngleDiff  = findMaxAngleDiff movs
          duration      = let detStart = (fst . snd) $ head movs
                              detEnd   = (snd . snd) $ last movs
                          in detDelta detStart detEnd

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

-- | Find movement sequences that are four movements or longer and whose
--   movements \"connect\" to each other.
-- 
-- \"Connection\" is defined as a closeness of the end point of
-- the earlier movement to the start point of the later movement.
-- This connection property is implemented in 'movsConnected'.
genMovChains :: [(Movement, (Detection, Detection))] -> [[(Movement, (Detection, Detection))]]
genMovChains movs = filter (\movs -> (length movs) >= 3) $ nonEmptySubMovChains (sortBy movStartTimeCompare movs)

nonEmptySubMovChains :: [(Movement, (Detection, Detection))]
                     -> [[(Movement, (Detection, Detection))]]
nonEmptySubMovChains [] = []
nonEmptySubMovChains (mov:movs) = [mov] : foldr f [] (nonEmptySubMovChains movs)
    where
      f [] r = [mov] : r
      f ys@(mov':movs') r = if (((fst $ snd mov) /= (fst $ snd mov')) -- movs do not start at same detection
                                && ((snd $ snd mov) /= (snd $ snd mov')) -- movs do not end at same detection
                                && (movsConnected mov mov'))
                            then ys : (mov : ys) : r
                            else ys : r

movStartTimeCompare :: (Movement, (Detection, Detection))
                    -> (Movement, (Detection, Detection))
                    -> Ordering
movStartTimeCompare (_, (detStart1, _)) (_, (detStart2, _)) =
    compare (detectionStartTime detStart1) (detectionStartTime detStart2)

-- | Calculate movement angle from latitude/longitude axis, in degrees
movAngle :: (Movement, (Detection, Detection))
         -> Double
movAngle (_, (detStart, detEnd))
    | tanPos && sinPos             = toDeg angle
    | (not tanPos) && sinPos       = 90.0 + (toDeg $ abs angle)
    | tanPos && (not sinPos)       = 180.0 + (toDeg angle)
    | (not tanPos) && (not sinPos) = 270.0 + (toDeg $ abs angle)
    where rise    = (detectionLat detEnd) - (detectionLat detStart)
          run     = (detectionLon detEnd) - (detectionLon detStart)
          angle   = atan $ rise / run
          tanPos  = angle > 0
          sinPos  = (sin angle) > 0
          toDeg x = 180.0 * x / 3.14159

movAngleDiff :: (Movement, (Detection, Detection))
             -> (Movement, (Detection, Detection))
             -> Double
movAngleDiff mov1 mov2 =
    let angle1 = movAngle mov1
        angle2 = movAngle mov2
    in min (abs $ angle2 - angle1) (abs $ angle1 + (360.0 - angle2))

-- | Find the largest angle change across a chain of movements.
-- 
-- The idea is to find near-180 degree reversals in a path.
findMaxAngleDiff :: [(Movement, (Detection, Detection))]
                 -> Double
findMaxAngleDiff []              = 0.0
findMaxAngleDiff (mov:[])        = 0.0
findMaxAngleDiff (mov:mov':[]) = movAngleDiff mov mov'
findMaxAngleDiff (mov:mov':mov'':movs) =
    maximum [movAngleDiff mov mov', movAngleDiff mov mov'', findMaxAngleDiff (mov':mov'':movs)]

-- | Determine if two movements are \"connected\".
movsConnected :: (Movement, (Detection, Detection))
              -> (Movement, (Detection, Detection))
              -> Bool
movsConnected mov1@(_, (detStart1, detEnd1)) mov2@(_, (detStart2, detEnd2)) =
    let testAngle = 60.0 > movAngleDiff mov1 mov2
        testDist  = detDist detEnd1 detStart2 <= 5.0
        testSeg   = detDistanceToSegment detStart1 detEnd1 detEnd2 < 70.0
        -- require that traveling from point 1 to 2 to 3 require less than 200% of 
        -- the distance than traveling from point 1 to 3 (take the most efficient route;
        -- may not always be the true route if the route was a short loop)
        testEff   = (((detDist detStart1 detEnd1) + (detDist detStart2 detEnd2))) <= (2.0 * (detDist detStart1 detEnd2))
        testSpeed = 100.0 > (abs $ (detSpeed detStart1 detEnd1) - (detSpeed detStart2 detEnd2))
    in testAngle && testDist && testSeg && testEff && testSpeed

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

-- | Test if the most recent movement of a path intersects a region
pathIntersectsRegion :: HypothesisMap Entity -> Path -> Region -> Bool
pathIntersectsRegion entityMap (Path _ (NonEmpty movRefs)) (Region _ (NonEmpty points)) =
    movIntersectsRegion points (getEntity entityMap (movementRefMovId $ last movRefs))
    where movIntersectsRegion :: [RegionPoint] -> Movement -> Bool
          movIntersectsRegion points (Movement _ detStartRef detEndRef _) =
              let detStart = getEntity entityMap detStartRef
                  detEnd   = getEntity entityMap detEndRef
              in (detInsideRegion points detStart) || (detInsideRegion points detEnd) ||
                     (detIsRegionPoint points detStart) || (detIsRegionPoint points detEnd)

detInsideRegion :: [RegionPoint] -> Detection -> Bool
detInsideRegion points det = detInsideRegion' (points ++ [head points]) det
    where
          -- from: http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/
          detInsideRegion' :: [RegionPoint] -> Detection -> Bool
          detInsideRegion' (_:[]) _ = False
          detInsideRegion' (p1:p2:ps) det@(Detection {detectionLat = dlat, detectionLon = dlon}) =
              let p1lat = regionPointLat p1
                  p1lon = regionPointLon p1
                  p2lat = regionPointLat p2
                  p2lon = regionPointLon p2
              in if (((p1lon <= dlon) && (dlon < p2lon)) ||
                     ((p2lon <= dlon) && (dlon < p1lon))) &&
                     (dlat < ((p2lat - p1lat) * (dlon - p1lon) / (p2lon - p1lon) + p1lat))
              then not $ detInsideRegion' (p2:ps) det
              else detInsideRegion' (p2:ps) det

detIsRegionPoint :: [RegionPoint] -> Detection -> Bool
detIsRegionPoint [] _ = False
detIsRegionPoint (p:ps) det@(Detection {detectionLat = dlat, detectionLon = dlon}) =
    ((dlat == (regionPointLat p)) && (dlon == (regionPointLon p))) ||
    detIsRegionPoint ps det

pathHeadNearPointOfInterest :: HypothesisMap Entity -> PointOfInterest -> Path -> (Bool, String)
pathHeadNearPointOfInterest entityMap (PointOfInterest name lat lon range) (Path _ (NonEmpty movRefs)) =
    let mov       = getEntity entityMap (movementRefMovId $ last movRefs)
        det1      = getEntity entityMap (movementDetId1 mov)
        det2      = getEntity entityMap (movementDetId2 mov)
        det1dist  = distance (lat, detectionLat det1) (lon, detectionLon det1)
        det2dist  = distance (lat, detectionLat det2) (lon, detectionLon det2)
        near      = range >= det2dist
        direction = if det2dist < det1dist then "away from" else "towards"
    in (near, direction)

pathAverageArea :: HypothesisMap Entity -> Path -> Double
pathAverageArea entityMap (Path _ (NonEmpty movRefs)) =
    let dets = nub $ foldl (\rest (detStart, detEnd) -> detStart:detEnd:rest) [] $
               map (\(Movement _ detStart detEnd _) ->
                        (getEntity entityMap detStart, getEntity entityMap detEnd)) $
               gatherEntities entityMap (IDSet.fromList $ map movementRefMovId movRefs)
    in (sum $ map detectionArea dets) / (fromIntegral $ length dets)

pathAverageSpeed :: HypothesisMap Entity -> Path -> Double
pathAverageSpeed entityMap (Path _ (NonEmpty movRefs)) =
    let detPairs = map (\(Movement _ detStart detEnd _) ->
                        (getEntity entityMap detStart, getEntity entityMap detEnd)) $
                   gatherEntities entityMap (IDSet.fromList $ map movementRefMovId movRefs)
    in foldl1 (+) $ map (uncurry detSpeed) detPairs

pathMatchesAgent :: HypothesisMap Entity -> Path -> Agent -> Level
pathMatchesAgent entityMap path (Agent _ area speed)
    | ratio >= 0.5  && ratio <= 2.0 = VeryHigh
    | ratio >= 0.25 && ratio <= 4.0 = High
    | otherwise                     = Low
    where areaRatio  = area / pathAverageArea entityMap path
          speedRatio = speed / pathAverageSpeed entityMap path
          ratio      = (areaRatio + speedRatio) / 2.0
