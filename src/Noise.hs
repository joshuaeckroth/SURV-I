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
import Data.List ((\\))

-- | Hypothesize and score noise
hypothesizeNoise :: CategoryID       -- ^ Hypothesis category
                 -> WorldState       -- ^ World state
                 -> World WorldState -- ^ Resulting world
hypothesizeNoise catID ws = hypothesizeNoise' catID (acqIDs ws) ws
    where 
      hypothesizeNoise' :: CategoryID -> [AcquisitionID] -> WorldState -> World WorldState
      hypothesizeNoise' catID []     ws = return ws
      hypothesizeNoise' catID (a:as) ws =
          hypothesizeNoise' catID as ws'
              where
                hypID       = nextHypID ws
                newHypIDs   = [hypID] ++ (hypIDs ws)
                explainID   = nextExplainer ws
                newNoiseIDs = (noiseIDs ws) ++ [hypID]
                newNoiseMap = IDMap.insert hypID a (noiseMap ws)
                newMind     = addExplains explainID hypID a
                              (addHypothesis hypID catID (scoreNoise hypID a (acqMap ws)) (mind ws))
                ws'         = ws { mind = newMind, hypIDs = newHypIDs, noiseIDs = newNoiseIDs, noiseMap = newNoiseMap }

scoreNoise :: HypothesisID -> AcquisitionID -> AcquisitionMap -> Level -> Level
scoreNoise hypID acqID acqMap _ =
    if (area acq) < 20.0 then High
    else Lowest
        where
          acq = IDMap.getItemFromMap acqMap acqID

showNoise :: [NoiseID] -> NoiseMap -> AcquisitionMap -> [String]
showNoise []     _         _       = []
showNoise (n:ns) noiseMap' acqMap' =
    ["Noise " ++ (show n) ++
     " [acquisition: " ++ (show a) ++ ", " ++
     "score: " ++ (show $ scoreNoise n a acqMap' Medium) ++ "]"]
    ++ showNoise ns noiseMap' acqMap'
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

-- | Keep only noise hypotheses considered \'irrefutable\' or \'accepted\'
updateNoise :: WorldState       -- ^ World state
            -> World WorldState -- ^ Resulting world
updateNoise ws =
    recordWorldEvent (["Removed noise:"] ++ (showNoise ((noiseIDs ws) \\ newNoiseIDs) (noiseMap ws) (acqMap ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, hypIDs = newHypIDs, noiseIDs = newNoiseIDs, noiseMap = newNoiseMap })
    where
      m             = mind ws
      frametime     = let (Frame attrs _) = (frame ws) in frameTime attrs
      goodHs        = IDSet.toList $ IDSet.union (irrefutableHypotheses m) (IDSet.union (acceptedHypotheses m) (consideringHypotheses m))
      -- filter out noise older than 1 sec
      freshNoiseMap = IDMap.filter (\acqID -> (frametime - (acquisitionTime (IDMap.getItemFromMap (acqMap ws) acqID))) < 1.0) (noiseMap ws)
      newNoiseMap   = IDMap.filterWithKey (\n _ -> elem n goodHs) freshNoiseMap
      newNoiseIDs   = filter (\n -> elem n $ IDMap.keys newNoiseMap) (noiseIDs ws)
      newHypIDs     = (hypIDs ws) \\ ((noiseIDs ws) \\ newNoiseIDs)
      newMind       = foldl (\m h -> removeHypothesis h m) (mind ws) ((noiseIDs ws) \\ newNoiseIDs)

recordNoise :: WorldState -> World WorldState
recordNoise ws =
    recordWorldEvent
    (showNoise ns noiseMap' acqMap',
     recordWorldEventInFrame framenum frametime $ noiseToXml ns noiseMap' acqMap') >>
    return ws
    where
      ns              = noiseIDs ws
      noiseMap'       = noiseMap ws
      acqMap'         = acqMap ws
      (Frame attrs _) = frame ws
      framenum        = show $ frameNumber attrs
      frametime       = show $ frameTime attrs

