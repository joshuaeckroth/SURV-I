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
    recordAcquisition f hypID a ws' >> hypothesizeAcquisitions catID f as ws'
        where
          hypID      = 1 + (head (hypIDs ws))
          newHypIDs  = [hypID] ++ (hypIDs ws)
          newAcqMap  = insert hypID a (acqMap ws)
          newAcqIDs  = (acqIDs ws) ++ [hypID]
          newMind    = setFactual (fromList [hypID])
                       (addHypothesis hypID catID (const Medium) (mind ws))
          ws'        = ws { mind = newMind, hypIDs = newHypIDs, acqMap = newAcqMap, acqIDs = newAcqIDs }

recordAcquisition :: Frame -> AcquisitionID -> Acquisition -> WorldState -> World WorldState
recordAcquisition (Frame attrs _) acqID a ws =
    recordWorldEvent ([showAcquisition acqID a],
                      recordWorldEventInFrame (show $ frameNumber attrs)
                                                  (show $ frameTime attrs)
                                                  [(worldElem "Acquisition" [("id", show $ acqID),
                                                                             ("x", show $ acquisitionX a),
                                                                             ("y", show $ acquisitionY a),
                                                                             ("width", show $ acquisitionWidth a),
                                                                             ("height", show $ acquisitionHeight a),
                                                                             ("area", show $ area a)]
                                                    [])])
                         >> return ws

showAcquisition :: AcquisitionID -> Acquisition -> String
showAcquisition acqID a = "Acquisition " ++ (show acqID) ++
                          " [x=" ++ (show $ acquisitionX a) ++ ", " ++
                          "y=" ++ (show $ acquisitionY a) ++ ", " ++
                          "width=" ++ (show $ acquisitionWidth a) ++ ", " ++
                          "height=" ++ (show $ acquisitionHeight a) ++ ", " ++
                          "area=" ++ (show $ area a) ++ "]"

area :: Acquisition -> Double
area Acquisition { acquisitionWidth = w, acquisitionHeight = h } = w * h

distance :: Acquisition -> Acquisition -> Double
distance Acquisition { acquisitionX = x1, acquisitionY = y1 }
         Acquisition { acquisitionX = x2, acquisitionY = y2 } =
    sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
