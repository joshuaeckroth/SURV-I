module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Data.List ((\\))
import Debug.Trace

mkPaths :: [Movement] -> [Hypothesis Path]
mkPaths movs = let movChains = genMovChains movs in map (mkPath movChains) movChains

mkPath :: [[Movement]] -> [Movement] -> Hypothesis Path
mkPath movChains movs =
    let hypId     = mkPathHypId movs
        path      = Path (Path_Attrs hypId) (NonEmpty movs)
        aPriori   = mkPathScore movs
        explains  = map (\(Movement (Movement_Attrs movId) _) -> movId) movs
        implies   = map (\(Movement (Movement_Attrs movId) _) -> movId) movs
        conflicts = []
    in Hyp path hypId aPriori explains implies conflicts

mkPathScore :: [Movement] -> (Level -> Level)
mkPathScore movs
    | avgChainDifferences < 5.0 && sumChainSpeedDifferences < 30.0 = (\s -> High)
    | otherwise = (\s -> Low)
    where avgChainDifferences      = (sumChainDifferences movs) / (fromIntegral $ length movs)
          avgChainSpeed            = (sum $ movChainSpeeds movs) / (fromIntegral $ length movs)
          sumChainSpeedDifferences = sum $ map (\s -> abs (s - avgChainSpeed)) $ movChainSpeeds movs

-- | Find the sum of differences (distance * time) of starting and ending
--   points of each link in a movement chain.
sumChainDifferences :: [Movement] -> Double
sumChainDifferences []     = 0.0
sumChainDifferences (_:[]) = 0.0
sumChainDifferences ((Movement _ (NonEmpty [_, detEnd])):
                     (Movement _ (NonEmpty [detStart, _])):movs) =
                        (detDist detEnd detStart) * (detDelta detEnd detStart) +
                        (sumChainDifferences movs)

-- | Find the speed each of link in the chain
movChainSpeeds :: [Movement] -> [Double]
movChainSpeeds [] = []
movChainSpeeds ((Movement _ (NonEmpty [det1, det2])):movs) =
    [(detDist det1 det2) / (detDelta det1 det2)] ++ (movChainSpeeds movs)

-- | Find movements that \"connect\" to each other.
-- 
-- \"Connection\" is defined as a closeness of the end point of
-- the earlier movement to the start point of the later movement.
-- This connection property is implemented in 'movsConnected'.
genMovChains :: [Movement] -> [[Movement]]
genMovChains [] = []
genMovChains (mov:[])   = [[mov]]
genMovChains (mov:movs) = [mov:rest | rest <- genMovChains (filter (movsConnected mov) movs)]
                            ++ (genMovChains movs)

-- | Determine if two movements are \"connected\".
-- 
-- Two movements are connected if the end point of the earlier movement
-- is with 5.0 units and 0.5 seconds of the start point of the later movement.
movsConnected :: Movement -> Movement -> Bool
movsConnected (Movement _ (NonEmpty [_, detEnd])) (Movement _ (NonEmpty [detStart, _])) =
    (detDist detEnd detStart < 10.0) && (detDelta detEnd detStart < 0.5)
                                         && (detBefore detEnd detStart)
