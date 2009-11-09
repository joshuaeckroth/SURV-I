module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List ((\\), sortBy)
import Debug.Trace

mkPaths :: HypothesisMap Entity -> [Movement] -> [Hypothesis Path]
mkPaths entityMap movs =
    let -- extract detections of movements for efficiency
        movsAndDets = map (\mov@(Movement _ detStart detEnd _) ->
                           (mov, (getEntity entityMap detStart, getEntity entityMap detEnd))) movs
        movChains   = genMovChains movsAndDets
    in map (mkPath movChains) movChains

mkPath :: [[(Movement, (Detection, Detection))]]
       -> [(Movement, (Detection, Detection))]
       -> Hypothesis Path
mkPath movChains movs =
    let hypId     = mkPathHypId (map fst movs)
        movRefs   = map extractMovHypId (map fst movs)
        score     = mkPathScore movs
        path      = Path (Path_Attrs hypId (show $ score Medium) "") (NonEmpty $ map mkMovementRef movRefs)
        aPriori   = score
        explains  = movRefs
        implies   = movRefs
        conflicts = []
    in Hyp path hypId aPriori explains implies conflicts

mkPathScore :: [(Movement, (Detection, Detection))] -> (Level -> Level)
mkPathScore movs
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 10.0 = (\s -> High)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 20.0 = (\s -> SlightlyHigh)
    | avgChainDiffs < 5.0 && sumSpeedDiffs < 40.0 = (\s -> Medium)
    | otherwise = (\s -> Low)
    where avgChainDiffs = (sumChainDifferences movs) / (fromIntegral $ length movs)
          sumSpeedDiffs = sumChainSpeedDifferences movs

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
    let connectedMovs = filter (\movs -> length movs > 2) $ nonEmptySubMovChains (sortBy movStartTimeCompare movs)
        -- movements that have a similar speed
        simSpeedMovs = filter (\movs -> sumChainSpeedDifferences movs < 50.0) connectedMovs
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
-- is with 2.0 units of the start point of the later movement.
movsSpatiallyConnected :: (Movement, (Detection, Detection))
                       -> (Movement, (Detection, Detection))
                       -> Bool
movsSpatiallyConnected (_, (_, detEnd)) (_, (detStart, _)) = detDist detEnd detStart < 2.0

movsTemporallyConnected :: (Movement, (Detection, Detection))
                        -> (Movement, (Detection, Detection))
                        -> Bool
movsTemporallyConnected (_, (_, detEnd)) (_, (detStart, _)) =
    let delta = detDelta detStart detEnd
    in delta < 0.5 && delta >= 0.0

