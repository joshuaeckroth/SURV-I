module Path
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))

mkPaths :: [Hypothesis Movement] -> [Hypothesis Path]
mkPaths hMovs =
    let movChains = genMovChains hMovs
    in map (mkPath movChains) movChains

mkPath :: [[Hypothesis Movement]] -> [Hypothesis Movement] -> Hypothesis Path
mkPath movChains movChain =
    let movs      = map extractMov movChain
        hypId     = mkPathHypId movs
        path      = Path (Path_Attrs hypId) (NonEmpty movs)
        aPriori   = (\_ -> High)
        explains  = [] :: [Hypothesis Movement]
        depends   = [] :: [Hypothesis Movement]
        conflicts = [] :: [Hypothesis Path]
    in Hyp path hypId aPriori explains depends conflicts

genMovChains :: [Hypothesis Movement] -> [[Hypothesis Movement]]
genMovChains [] = []
genMovChains (mov:movs) = [mov:rest | rest <- genMovChains $ filter (movsConnected mov) movs]
                          ++ (genMovChains movs)

movsConnected :: Hypothesis Movement -> Hypothesis Movement -> Bool
movsConnected (Hyp (Movement _ (NonEmpty [_, detEnd])) _ _ _ _ _)
              (Hyp (Movement _ (NonEmpty [detStart, _])) _ _ _ _ _) =
                  (detDist detEnd detStart < 150.0)
                  && (detDelta detEnd detStart < 2.0)
                         && (detBefore detEnd detStart)

extractMov :: Hypothesis Movement -> Movement
extractMov (Hyp mov _ _ _ _ _) = mov
