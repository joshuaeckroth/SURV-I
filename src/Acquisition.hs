module Acquisition where
import Types
import World
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types (HasMap)
import WrappedInts.IDSet (fromList)
import WrappedInts.IDMap (insert, getItemFromMap)
import Text.XML.HaXml.Types as HaXml

hypothesizeAcquisitions :: CategoryID
                        -> Frame
                        -> [Acquisition]
                        -> WorldState
                        -> World WorldState
hypothesizeAcquisitions _     _ []     ws = return ws
hypothesizeAcquisitions catID f (a:as) ws =
    hypothesizeAcquisitions catID f as ws'
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

area :: Acquisition -> Double
area Acquisition { acquisitionWidth = w, acquisitionHeight = h } = w * h

distance :: Acquisition -> Acquisition -> Double
distance Acquisition { acquisitionX = x1, acquisitionY = y1 }
         Acquisition { acquisitionX = x2, acquisitionY = y2 } =
    sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
