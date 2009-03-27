module Noise where
import Types
import World
import Detection
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import qualified Data.IntSet as IntSet (findMax)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import WrappedInts.Types
import Text.XML.HaXml.Types as HaXml
import Data.List ((\\))

-- | Hypothesize and score noise
hypothesizeNoise :: CategoryID       -- ^ Hypothesis category
                 -> WorldState       -- ^ World state
                 -> World WorldState -- ^ Resulting world
hypothesizeNoise catID ws = hypothesizeNoise' catID (detIDs ws) ws
    where 
      hypothesizeNoise' :: CategoryID -> [DetectionID] -> WorldState -> World WorldState
      hypothesizeNoise' catID []         ws = return ws
      hypothesizeNoise' catID (did:dids) ws =
          hypothesizeNoise' catID dids ws'
              where
                hypID       = nextHypID ws
                newHypIDs   = [hypID] ++ (hypIDs ws)
                explainID   = nextExplainer ws
                newNoiseIDs = (noiseIDs ws) ++ [hypID]
                newNoiseMap = IDMap.insert hypID did (noiseMap ws)
                newMind     = addExplains explainID hypID did
                              (addHypothesis hypID catID (scoreNoise hypID did (detMap ws)) (mind ws))
                ws'         = ws { mind = newMind, hypIDs = newHypIDs, noiseIDs = newNoiseIDs, noiseMap = newNoiseMap }

scoreNoise :: HypothesisID -> DetectionID -> DetectionMap -> Level -> Level
scoreNoise hypID did m _ =
    if area < 20.0 then High
    else Lowest
        where
          Detection (Detection_Attrs { detArea = area }) = IDMap.getItemFromMap m did

showNoise :: [NoiseID] -> NoiseMap -> DetectionMap -> [String]
showNoise []     _         _       = []
showNoise (n:ns) nm dm =
    ["Noise " ++ (show n) ++
     " [detection: " ++ (show did) ++ ", " ++
     "score: " ++ (show $ scoreNoise n did dm Medium) ++ "]"]
    ++ showNoise ns nm dm
    where
      did = IDMap.getItemFromMap nm n

noiseToXml :: [NoiseID] -> NoiseMap -> DetectionMap -> [HaXml.Content ()]
noiseToXml []     _  _  = []
noiseToXml (n:ns) nm dm =
    [worldElem "Noise" [("id", show n),
                        ("camera", detProp detCamera det),
                        ("cx", show $ detProp detCx det),
                        ("cy", show $ detProp detCy det),
                        ("area", show $ detProp detArea det)] []]
    ++ noiseToXml ns nm dm
    where
      det = IDMap.getItemFromMap dm (IDMap.getItemFromMap nm n)

updateNoise :: WorldState       -- ^ World state
            -> World WorldState -- ^ Resulting world
updateNoise ws =
    recordWorldEvent (["Removed noise:"] ++ (showNoise ((noiseIDs ws) \\ newNoiseIDs) (noiseMap ws) (detMap ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, hypIDs = newHypIDs, noiseIDs = newNoiseIDs, noiseMap = newNoiseMap })
    where
      m             = mind ws
      goodHs        = IDSet.toList $ IDSet.union (irrefutableHypotheses m) (IDSet.union (acceptedHypotheses m) (consideringHypotheses m))
      newNoiseMap   = IDMap.filterWithKey (\n _ -> elem n goodHs) (noiseMap ws)
      newNoiseIDs   = filter (\n -> elem n $ IDMap.keys newNoiseMap) (noiseIDs ws)
      newHypIDs     = (hypIDs ws) \\ ((noiseIDs ws) \\ newNoiseIDs)
      newMind       = foldl (\m h -> removeHypothesis h m) (mind ws) ((noiseIDs ws) \\ newNoiseIDs)

recordNoise :: WorldState -> World WorldState
recordNoise ws =
    recordWorldEvent
    (showNoise ns nm dm,
     recordWorldEventInFrame frame $ noiseToXml ns nm dm) >>
    return ws
    where
      ns        = noiseIDs ws
      nm        = noiseMap ws
      dm        = detMap ws
      frame     = curFrame ws

