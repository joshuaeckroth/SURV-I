module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkPaths :: [Hypothesis Movement] -> [Hypothesis Path]
mkPaths hMovs =
    let movChains = genMovChains $ map extractMov hMovs
    in map (mkPath movChains) (traceShow movChains movChains)

mkPath :: [[Movement]] -> [Movement] -> Hypothesis Path
mkPath movChains movs =
    let hypId     = mkPathHypId movs
        path      = Path (Path_Attrs hypId) (NonEmpty movs)
        aPriori   = (\_ -> High)
        explains  = [] :: [Hypothesis Movement]
        depends   = [] :: [Hypothesis Movement]
        conflicts = [] :: [Hypothesis Path]
    in Hyp path hypId aPriori explains depends conflicts

genMovChains :: [Movement] -> [[Movement]]
genMovChains [] = []
genMovChains (mov:movs) = [rest | rest <- genMovChains (filter (movsConnected mov) movs)]
                          ++ (genMovChains movs)

movsConnected :: Movement -> Movement -> Bool
movsConnected (Movement _ (NonEmpty [_, detEnd])) (Movement _ (NonEmpty [detStart, _])) =
    (detDist detEnd detStart < 150.0) && (detDelta detEnd detStart < 2.0)
                                          && (detBefore detEnd detStart)

extractMov :: Hypothesis Movement -> Movement
extractMov (Hyp mov _ _ _ _ _) = mov
