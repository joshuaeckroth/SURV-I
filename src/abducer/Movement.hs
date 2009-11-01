module Movement
where

import Types
import Vocabulary
import Text.XML.HaXml.XmlContent.Parser (List1(..))

mkMovements :: [Hypothesis Detection] -> [Hypothesis Movement]
mkMovements hDets =
    let closeDetPairs = [(hdet1, hdet2, dist, delta) |
                         hdet1@(Hyp { entity = det1 }) <- hDets,
                         hdet2@(Hyp { entity = det2 }) <- hDets,
                         let dist  = detDist det1 det2,
                         let delta = detDelta det1 det2,
                         det1 /= det2,
                         dist < 300.0, delta < 5.0, detBefore det1 det2]
    in map (mkMovement closeDetPairs) closeDetPairs

mkMovement :: [(Hypothesis Detection, Hypothesis Detection, Double, Time)]
           -> (Hypothesis Detection, Hypothesis Detection, Double, Time)
           -> Hypothesis Movement
mkMovement hdets (hdet1@(Hyp { entity = det1 }),
                  hdet2@(Hyp { entity = det2 }), dist, delta) =
    let hypId     = mkMovHypId [det1, det2]
        mov       = Movement (Movement_Attrs hypId) (NonEmpty [det1, det2])
        aPriori   = mkMovementScore hdets (hdet1, hdet2, dist, delta)
        explains  = [hdet1, hdet2]
        depends   = [hdet1, hdet2]
        conflicts = [] :: [Hypothesis Detection] {- arbitrary type signature -}
    in Hyp mov hypId aPriori explains depends conflicts

mkMovementScore :: [(Hypothesis Detection, Hypothesis Detection, Double, Time)]
                -> (Hypothesis Detection, Hypothesis Detection, Double, Time)
                -> (Level -> Level)
mkMovementScore hdets (hdet1@(Hyp { entity = det1 }),
                       hdet2@(Hyp { entity = det2 }), dist, delta)
    | dist < 100.0 && delta < 0.5 = (\s -> corroboration High)
    | dist < 150.0 && delta < 1.0 = (\s -> corroboration Medium)
    | otherwise                   = (\s -> corroboration Low)
    where corroboration = id {- FIXME -}