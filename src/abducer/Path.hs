module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List ((\\), subsequences, sortBy)
import Debug.Trace

mkPaths :: HypothesisMap Entity -> [Movement] -> [Hypothesis Path]
mkPaths entityMap movs =
    let movChains = genMovChains entityMap movs
    in map (mkPath entityMap movChains) movChains

mkPath :: HypothesisMap Entity -> [[Movement]] -> [Movement] -> Hypothesis Path
mkPath entityMap movChains movs =
    let hypId     = mkPathHypId movs
        movRefs   = map extractMovHypId movs
        score     = mkPathScore entityMap movs
        path      = Path (Path_Attrs hypId (show $ score Medium)) (NonEmpty $ map mkMovementRef movRefs)
        aPriori   = score
        explains  = movRefs
        implies   = movRefs
        conflicts = []
    in Hyp path hypId aPriori explains implies conflicts

mkPathScore :: HypothesisMap Entity -> [Movement] -> (Level -> Level)
mkPathScore entityMap movs
    | avgChainDifferences < 5.0 && sumChainSpeedDifferences < 10.0 = (\s -> High)
    | avgChainDifferences < 5.0 && sumChainSpeedDifferences < 20.0 = (\s -> SlightlyHigh)
    | avgChainDifferences < 5.0 && sumChainSpeedDifferences < 40.0 = (\s -> Medium)
    | otherwise = (\s -> Low)
    where avgChainDifferences      = (sumChainDifferences entityMap movs) / (fromIntegral $ length movs)
          avgChainSpeed            = (sum $ movChainSpeeds entityMap movs) / (fromIntegral $ length movs)
          sumChainSpeedDifferences = sum $ map (\s -> abs (s - avgChainSpeed)) $ movChainSpeeds entityMap movs

-- | Find the sum of differences (distance * time) of starting and ending
--   points of each link in a movement chain.
sumChainDifferences :: HypothesisMap Entity -> [Movement] -> Double
sumChainDifferences _         []     = 0.0
sumChainDifferences _         (_:[]) = 0.0
sumChainDifferences entityMap ((Movement _ _ detEndHypId _):(Movement _ detStartHypId _ _):movs) =
    let detEnd   = getEntity entityMap detEndHypId
        detStart = getEntity entityMap detStartHypId
    in (detDist detEnd detStart) * (detDelta detEnd detStart) + (sumChainDifferences entityMap movs)

-- | Find the speed each of link in the chain
movChainSpeeds :: HypothesisMap Entity -> [Movement] -> [Double]
movChainSpeeds _ [] = []
movChainSpeeds entityMap ((Movement _ det1HypId det2HypId _):movs) =
    let det1 = getEntity entityMap det1HypId
        det2 = getEntity entityMap det2HypId
    in [(detDist det2 det1) / (detDelta det2 det1)] ++ (movChainSpeeds entityMap movs)

-- | Find movement sequences that are longer than two movements and whose
--   movements \"connect\" to each other.
-- 
-- \"Connection\" is defined as a closeness of the end point of
-- the earlier movement to the start point of the later movement.
-- This connection property is implemented in 'movsSpatiallyConnected'
-- and 'movsTemporallyConnected'; we do this successive filtering
-- for efficiency.
genMovChains :: HypothesisMap Entity -> [Movement] -> [[Movement]]
genMovChains entityMap movs = 
    let sortedMovs   = filter (\movs -> length movs > 2) $
                       subsequences (sortBy (movStartTimeCompare entityMap) movs)
        temporalMovs = filter (\movs -> and $ zipWith (\mov1 mov2 -> movsTemporallyConnected entityMap mov1 mov2)
                                        movs (tail movs)) sortedMovs
        spatialMovs  = filter (\movs -> and $ zipWith (\mov1 mov2 -> movsSpatiallyConnected entityMap mov1 mov2)
                                        movs (tail movs)) temporalMovs
    in spatialMovs

movStartTimeCompare :: HypothesisMap Entity -> Movement -> Movement -> Ordering
movStartTimeCompare entityMap mov1 mov2 =
    let detStart1 = getEntity entityMap (movementDetId1 mov1)
        detStart2 = getEntity entityMap (movementDetId1 mov2)
    in compare (detectionStartTime detStart1) (detectionStartTime detStart2)

-- | Determine if two movements are \"connected\".
-- 
-- Two movements are connected if the end point of the earlier movement
-- is with 2.0 units of the start point of the later movement.
movsSpatiallyConnected :: HypothesisMap Entity -> Movement -> Movement -> Bool
movsSpatiallyConnected entityMap (Movement _ _ detEndHypId _) (Movement _ detStartHypId _ _) =
    let detEnd   = getEntity entityMap detEndHypId
        detStart = getEntity entityMap detStartHypId
    in detDist detEnd detStart < 2.0

movsTemporallyConnected :: HypothesisMap Entity -> Movement -> Movement -> Bool
movsTemporallyConnected entityMap (Movement _ _ detEndHypId _) (Movement _ detStartHypId _ _) =
    let detStart = getEntity entityMap detStartHypId
        detEnd   = getEntity entityMap detEndHypId
        delta    = detDelta detStart detEnd
    in delta < 0.5 && delta >= 0.0

-- | Check if a path is shorter than 3.0 seconds or contains only one movement
isShortMovChain :: HypothesisMap Entity -> [Movement] -> Bool
isShortMovChain _ []       = True -- no movements is a "short chain"
isShortMovChain _ (mov:[]) = True -- one movement is a short chain
isShortMovChain entityMap movs =
    let (Movement _ detStartHypId _ _) = head movs -- earliest
        (Movement _ _ detEndHypId _)   = last movs -- most recent
        detStart                     = getEntity entityMap detStartHypId
        detEnd                       = getEntity entityMap detEndHypId
    in 3.0 < (detDelta detEnd detStart)
