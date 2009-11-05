module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkPaths :: [Movement] -> [Hypothesis Path]
mkPaths movs =
    let movChains = genMovChains movs
    in map (mkPath movChains) movChains

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
mkPathScore movs = (\_ -> Medium)

genMovChains :: [Movement] -> [[Movement]]
genMovChains [] = []
genMovChains (mov:[])   = [[mov]]
genMovChains (mov:movs) = [mov:rest | rest <- genMovChains (filter (movsConnected mov) movs)]
                            ++ (genMovChains movs)

movsConnected :: Movement -> Movement -> Bool
movsConnected (Movement _ (NonEmpty [_, detEnd])) (Movement _ (NonEmpty [detStart, _])) =
    (detDist detEnd detStart < 10.0) && (detDelta detEnd detStart < 2.0)
                                         && (detBefore detEnd detStart)

