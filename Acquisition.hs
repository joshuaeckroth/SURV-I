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
                        -> [Acquisition]
                        -> WorldState
                        -> World WorldState
hypothesizeAcquisitions _     []     ws = recordAcquisitions ws >> return ws
hypothesizeAcquisitions catID (a:as) ws =
    hypothesizeAcquisitions catID as ws'
        where
          hypID      = 1 + (head (hypIDs ws))
          newHypIDs  = [hypID] ++ (hypIDs ws)
          newAcqMap  = insert hypID a (acqMap ws)
          newAcqIDs  = (acqIDs ws) ++ [hypID]
          newMind    = setFactual (fromList [hypID])
                       (addHypothesis hypID catID (const Medium) (mind ws))
          ws'        = ws { mind = newMind, hypIDs = newHypIDs, acqMap = newAcqMap, acqIDs = newAcqIDs }

recordAcquisitions :: WorldState -> World WorldState
recordAcquisitions ws = recordWorldEvent
                        (showAcquisitions (acqIDs ws) (acqMap ws), HaXml.Elem "Acquisition" [] [])
                        >> return ws

showAcquisitions :: [AcquisitionID] -> AcquisitionMap -> [String]
showAcquisitions []     _      = []
showAcquisitions (a:as) am = ("Add as fact: Acquisition " ++ (show a) ++
                          " [x=" ++ (show $ acquisitionX acq) ++ ", " ++
                          "y=" ++ (show $ acquisitionY acq) ++ ", " ++
                          "width=" ++ (show $ acquisitionWidth acq) ++ ", " ++
                          "height=" ++ (show $ acquisitionHeight acq) ++ ", " ++
                          "area=" ++ (show $ area acq) ++ "]") :
                          showAcquisitions as am
                              where
                                acq = getItemFromMap am a

area :: Acquisition -> Double
area Acquisition { acquisitionWidth = w, acquisitionHeight = h } = w * h

distance :: Acquisition -> Acquisition -> Double
distance Acquisition { acquisitionX = x1, acquisitionY = y1 }
         Acquisition { acquisitionX = x2, acquisitionY = y2 } =
    sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
