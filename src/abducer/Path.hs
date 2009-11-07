module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List ((\\))
import Debug.Trace

mkPaths :: HypothesisMap Entity -> [Movement] -> [Hypothesis Path]
mkPaths entityMap movs = let movChains = filter (not . (isShortMovChain entityMap))
                                         (genMovChains entityMap movs)
                         in map (mkPath entityMap movChains) movChains

mkPath :: HypothesisMap Entity -> [[Movement]] -> [Movement] -> Hypothesis Path
mkPath entityMap movChains movs =
    let hypId     = mkPathHypId movs
        movRefs   = map extractMovHypId movs
        path      = Path (Path_Attrs hypId) (NonEmpty $ map mkMovementRef movRefs)
        aPriori   = mkPathScore entityMap movs
        explains  = movRefs
        implies   = movRefs
        conflicts = []
    in Hyp path hypId aPriori explains implies conflicts

mkPathScore :: HypothesisMap Entity -> [Movement] -> (Level -> Level)
mkPathScore entityMap movs
    | {-- avgChainDifferences < 5.0 && --} sumChainSpeedDifferences < 300.0 = (\s -> High)
    | otherwise = (\s -> Low)
    where avgChainDifferences      = (sumChainDifferences entityMap movs) / (fromIntegral $ length movs)
          avgChainSpeed            = (sum $ movChainSpeeds entityMap movs) / (fromIntegral $ length movs)
          sumChainSpeedDifferences = sum $ map (\s -> abs (s - avgChainSpeed)) $ movChainSpeeds entityMap movs

-- | Find the sum of differences (distance * time) of starting and ending
--   points of each link in a movement chain.
sumChainDifferences :: HypothesisMap Entity -> [Movement] -> Double
sumChainDifferences _         []     = 0.0
sumChainDifferences _         (_:[]) = 0.0
sumChainDifferences entityMap ((Movement _ _ detEndHypId):(Movement _ _ detStartHypId):movs) =
    let detEnd   = getEntity entityMap detEndHypId
        detStart = getEntity entityMap detStartHypId
    in (detDist detEnd detStart) * (detDelta detEnd detStart) + (sumChainDifferences entityMap movs)

-- | Find the speed each of link in the chain
movChainSpeeds :: HypothesisMap Entity -> [Movement] -> [Double]
movChainSpeeds _ [] = []
movChainSpeeds entityMap ((Movement _ det1HypId det2HypId):movs) =
    let det1 = getEntity entityMap det1HypId
        det2 = getEntity entityMap det2HypId
    in [(detDist det2 det1) / (detDelta det2 det1)] ++ (movChainSpeeds entityMap movs)

-- | Find movements that \"connect\" to each other.
-- 
-- \"Connection\" is defined as a closeness of the end point of
-- the earlier movement to the start point of the later movement.
-- This connection property is implemented in 'movsSpatiallyConnected'
-- and 'movsTemporallyConnected'; we do this successive filtering
-- for efficiency.
genMovChains :: HypothesisMap Entity -> [Movement] -> [[Movement]]
genMovChains _ [] = []
genMovChains _ (mov:[]) = [[mov]]
genMovChains entityMap (mov:movs) = [mov:rest | rest <- genMovChains entityMap
                                                        (filter (movsTemporallyConnected entityMap mov)
                                                         (filter (movsSpatiallyConnected entityMap mov) movs))]
                                    ++ (genMovChains entityMap movs)

-- | Determine if two movements are \"connected\".
-- 
-- Two movements are connected if the end point of the earlier movement
-- is with 2.0 units of the start point of the later movement.
movsSpatiallyConnected :: HypothesisMap Entity -> Movement -> Movement -> Bool
movsSpatiallyConnected entityMap (Movement _ _ detEndHypId) (Movement _ detStartHypId _) =
    let detEnd   = getEntity entityMap detEndHypId
        detStart = getEntity entityMap detStartHypId
    in detDist detEnd detStart < 2.0

movsTemporallyConnected :: HypothesisMap Entity -> Movement -> Movement -> Bool
movsTemporallyConnected entityMap (Movement _ _ detEndHypId) (Movement _ detStartHypId _) =
    let detStart = getEntity entityMap detStartHypId
        detEnd   = getEntity entityMap detEndHypId
        delta    = detDelta detStart detEnd
    in delta < 0.5 && delta >= 0.0

-- | Check if a path is shorter than 3.0 seconds
isShortMovChain :: HypothesisMap Entity -> [Movement] -> Bool
isShortMovChain _ [] = True
isShortMovChain entityMap movs =
    let (Movement _ detStartHypId _) = head movs -- earliest
        (Movement _ _ detEndHypId)   = last movs -- most recent
        detStart                     = getEntity entityMap detStartHypId
        detEnd                       = getEntity entityMap detEndHypId
    in 3.0 < (detDelta detEnd detStart)
