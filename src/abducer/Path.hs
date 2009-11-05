module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Debug.Trace

mkPaths :: [Hypothesis Movement] -> [Hypothesis Path]
mkPaths hMovs =
    let hMovChains = genMovChains hMovs
    in map (mkPath hMovChains) hMovChains

mkPath :: [[Hypothesis Movement]] -> [Hypothesis Movement] -> Hypothesis Path
mkPath hMovChains hMovs =
    let movs      = map (\(Hyp {entity = mov}) -> mov) hMovs
        hypId     = mkPathHypId movs
        path      = Path (Path_Attrs hypId) (NonEmpty movs)
        aPriori   = (\_ -> High) {-- FIXME --}
        explains  = hMovs
        implies   = hMovs
        conflicts = [] :: [Hypothesis Path] {-- FIXME --}
    in Hyp path hypId aPriori explains implies conflicts

genMovChains :: [Hypothesis Movement] -> [[Hypothesis Movement]]
genMovChains [] = []
genMovChains (hMov:[])    = [[hMov]]
genMovChains (hMov:hMovs) = [hMov:rest | rest <- genMovChains (filter (movsConnected hMov) hMovs)]
                            ++ (genMovChains hMovs)

movsConnected :: Hypothesis Movement -> Hypothesis Movement -> Bool
movsConnected (Hyp {entity = (Movement _ (NonEmpty [_, detEnd]))})
                  (Hyp {entity = (Movement _ (NonEmpty [detStart, _]))}) =
                      (detDist detEnd detStart < 10.0) && (detDelta detEnd detStart < 2.0)
                                                           && (detBefore detEnd detStart)

