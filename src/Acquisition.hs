module Acquisition where
import Types
import World
import Frame
import Reasoner.Core
import Reasoner.Types
import Reasoner.Constrainers
import Vocabulary
import WrappedInts.Types (HasMap)
import WrappedInts.IDSet (fromList)
import WrappedInts.IDMap (insert, getItemFromMap, keys)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import Text.XML.HaXml.Types as HaXml

-- | Hypothesize acquisitions and declare each as factual
hypothesizeAcquisitions :: CategoryID       -- ^ Hypothesis category
                        -> WorldState       -- ^ World state
                        -> World WorldState -- ^ Resulting world
hypothesizeAcquisitions catID ws = hypothesizeAcquisitions' catID (getAcquisitions (frame ws)) ws
    where
      hypothesizeAcquisitions' _     []     ws = return ws
      hypothesizeAcquisitions' catID (a:as) ws =
          hypothesizeAcquisitions' catID as ws'
              where
                hypID      = 1 + (head (hypIDs ws))
                newHypIDs  = [hypID] ++ (hypIDs ws)
                newAcqIDs  = (acqIDs ws) ++ [hypID]
                newAcqMap  = insert hypID a (acqMap ws)
                newMind    = setFactual (fromList [hypID])
                             (addHypothesis hypID catID (const Medium) (mind ws))
                ws'        = ws { mind = newMind, hypIDs = newHypIDs, acqMap = newAcqMap, acqIDs = newAcqIDs }

showAcquisitions :: [AcquisitionID] -> AcquisitionMap -> [String]
showAcquisitions []     _       = []
showAcquisitions (a:as) acqMap' =
    ["Acquisition " ++ (show a) ++
     " [x=" ++ (show $ acquisitionX acq) ++ ", " ++
     "y=" ++ (show $ acquisitionY acq) ++ ", " ++
     "width=" ++ (show $ acquisitionWidth acq) ++ ", " ++
     "height=" ++ (show $ acquisitionHeight acq) ++ ", " ++
     "area=" ++ (show $ area acq) ++ "]"]
    ++ showAcquisitions as acqMap'
    where
      acq = getItemFromMap acqMap' a

acquisitionsToXml :: [AcquisitionID] -> AcquisitionMap -> [HaXml.Content]
acquisitionsToXml []     _       = []
acquisitionsToXml (a:as) acqMap' =
    [worldElem "Acquisition" [("id", show a),
                              ("x", show $ acquisitionX acq),
                              ("y", show $ acquisitionY acq),
                              ("width", show $ acquisitionWidth acq),
                              ("height", show $ acquisitionHeight acq),
                              ("area", show $ area acq)] []]
    ++ acquisitionsToXml as acqMap'
    where
      acq = getItemFromMap acqMap' a

recordAcquisitions :: WorldState -> World WorldState
recordAcquisitions ws =
    recordWorldEvent (showAcquisitions as acqMap',
                      recordWorldEventInFrame framenum frametime $ acquisitionsToXml as acqMap') >>
    return ws
    where
      as              = acqIDs ws
      acqMap'         = acqMap ws
      (Frame attrs _) = frame ws
      framenum        = show $ frameNumber attrs
      frametime       = show $ frameTime attrs

-- | Add constraints for all hypotheses explaining the same acquisition
--
-- Check noise and track hypotheses. The constraint is a 'Reasoner.Constraints.oneOf' constraint
-- which allows keeping only one of the hypotheses per acquisition.
constrainAcquisitionExplainers :: WorldState       -- ^ World state
                               -> World WorldState -- ^ Resulting world
constrainAcquisitionExplainers ws = constrainAcquisitionExplainers' (acqIDs ws) ws
    where
      constrainAcquisitionExplainers' :: [AcquisitionID] -> WorldState -> World WorldState
      constrainAcquisitionExplainers' []              ws = return ws
      constrainAcquisitionExplainers' (acqID:acqIDs') ws =
          constrainAcquisitionExplainers' acqIDs' ws'
              where
                acq       = getItemFromMap (acqMap ws) acqID
                noiseIDs' = keys (IDMap.filter ((==) acqID) (noiseMap ws))
                trackIDs' = keys (IDMap.filter (\(Track acq' _) -> acq' == acq) (trackMap ws))
                -- the track hypotheses will be constrained by the noise hypotheses
                ws'       = constrain trackIDs' noiseIDs' acqID ws
                constrain :: [HypothesisID] -> [HypothesisID] -> AcquisitionID -> WorldState -> WorldState
                constrain []     _       _     ws'' = ws''
                constrain (h:hs) hypIDs' acqID ws'' = constrain hs hypIDs' acqID ws'''
                    where
                      ws''' = ws'' { mind = addConstrainer (nextConstrainer ws'') (IDSet.fromList hypIDs') oneOf h (mind ws'') }

area :: Acquisition -> Double
area Acquisition { acquisitionWidth = w, acquisitionHeight = h } = w * h

distance :: Acquisition -> Acquisition -> Double
distance Acquisition { acquisitionX = x1, acquisitionY = y1 }
         Acquisition { acquisitionX = x2, acquisitionY = y2 } =
    sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
