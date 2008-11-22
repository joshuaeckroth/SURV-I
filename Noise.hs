module Noise where
import Types
import World
import Acquisition
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import qualified Data.IntSet as IntSet (findMax)
import qualified WrappedInts.IDMap as IDMap
import WrappedInts.Types

hypothesizeNoise :: CategoryID
                 -> WorldState
                 -> World WorldState
hypothesizeNoise catID ws = hypothesizeNoise' catID (acqIDs ws) ws

hypothesizeNoise' catID []     ws = return ws
hypothesizeNoise' catID (a:as) ws =
    hypothesizeNoise' catID as ws'
        where
          hypID      = 1 + (head (hypIDs ws))
          newHypIDs  = [hypID] ++ (hypIDs ws)
          explainID  = HasInt (nextID explainers (mind ws))
          newMind    = addExplains explainID hypID a
                       (addHypothesis hypID catID (scoreNoiseHypothesis hypID a (acqMap ws)) (mind ws))
          ws'        = ws { mind = newMind, hypIDs = newHypIDs }

scoreNoiseHypothesis :: HypothesisID -> AcquisitionID -> AcquisitionMap -> Level -> Level
scoreNoiseHypothesis hypID acqID acqMap _ =
    if (area acq) < 20.0 then Highest
    else Lowest
        where
          acq = IDMap.getItemFromMap acqMap acqID

nextID field mind = if (null . IDMap.toList) (field mind) then 1
                    else 1 + (IntSet.findMax . wrappedInts . IDMap.keysSet) (field mind)

