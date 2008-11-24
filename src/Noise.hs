module Noise where
import Types
import World
import Acquisition
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import qualified Data.IntSet as IntSet (findMax)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import WrappedInts.Types
import Text.XML.HaXml.Types as HaXml

hypothesizeNoise :: CategoryID
                 -> WorldState
                 -> World WorldState
hypothesizeNoise catID ws = hypothesizeNoise' catID (acqIDs ws) ws

hypothesizeNoise' catID []     ws = return ws
hypothesizeNoise' catID (a:as) ws =
    hypothesizeNoise' catID as ws'
        where
          hypID       = 1 + (head (hypIDs ws))
          newHypIDs   = [hypID] ++ (hypIDs ws)
          explainID   = nextID explainers (mind ws)
          newNoiseIDs = (noiseIDs ws) ++ [hypID]
          newNoiseMap = IDMap.insert hypID a (noiseMap ws)
          newMind     = addExplains explainID hypID a
                        (addHypothesis hypID catID (scoreNoiseHypothesis hypID a (acqMap ws)) (mind ws))
          ws'         = ws { mind = newMind, hypIDs = newHypIDs, noiseIDs = newNoiseIDs, noiseMap = newNoiseMap }

scoreNoiseHypothesis :: HypothesisID -> AcquisitionID -> AcquisitionMap -> Level -> Level
scoreNoiseHypothesis hypID acqID acqMap _ =
    if (area acq) < 20.0 then Highest
    else Lowest
        where
          acq = IDMap.getItemFromMap acqMap acqID

showNoise :: [NoiseID] -> NoiseMap -> [String]
showNoise []     _         = []
showNoise (n:ns) noiseMap' =
    ["Noise " ++ (show n) ++
     " (acquisition: " ++ (show a) ++ ")"]
    ++ showNoise ns noiseMap'
    where
      a = IDMap.getItemFromMap noiseMap' n

noiseToXml :: [NoiseID] -> NoiseMap -> AcquisitionMap -> [HaXml.Content]
noiseToXml []     _         _       = []
noiseToXml (n:ns) noiseMap' acqMap' =
    [worldElem "Noise" [("id", show n),
                        ("x", show $ acquisitionX acq),
                        ("y", show $ acquisitionY acq),
                        ("width", show $ acquisitionWidth acq),
                        ("height", show $ acquisitionHeight acq),
                        ("area", show $ area acq)] []]
    ++ noiseToXml ns noiseMap' acqMap'
    where
      acq = IDMap.getItemFromMap acqMap' (IDMap.getItemFromMap noiseMap' n)

updateNoise :: WorldState -> World WorldState
updateNoise ws = return (ws { noiseIDs = newNoiseIDs, noiseMap = newNoiseMap })
    where
      m           = mind ws
      hs          = IDSet.toList $ IDSet.union (irrefutableHypotheses m) (acceptedHypotheses m)
      -- only keep accepted or irrefutable noise hypotheses
      newNoiseMap = IDMap.filterWithKey (\n _ -> elem n hs) (noiseMap ws)
      newNoiseIDs = filter (\n -> elem n $ IDMap.keys newNoiseMap) (noiseIDs ws)

recordNoise :: WorldState -> World WorldState
recordNoise ws =
    recordWorldEvent
    (showNoise ns noiseMap',
     recordWorldEventInFrame framenum frametime $ noiseToXml ns noiseMap' acqMap') >>
    return ws
    where
      ns              = noiseIDs ws
      noiseMap'       = noiseMap ws
      acqMap'         = acqMap ws
      (Frame attrs _) = frame ws
      framenum        = show $ frameNumber attrs
      frametime       = show $ frameTime attrs

