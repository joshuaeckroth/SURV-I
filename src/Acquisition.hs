module Acquisition where
import Types
import World
import Frame
import Reasoner.Core
import Reasoner.Types
import Reasoner.Constrainers
import Vocabulary
import WrappedInts.Types (HasInt(..))
import WrappedInts.IDSet (fromList)
import WrappedInts.IDMap (insert, getItemFromMap, keys)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import Text.XML.HaXml.Types as HaXml
import Data.List ((\\))

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
                hypID      = nextHypID ws
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

updateAcquisitions :: WorldState -> World WorldState
updateAcquisitions ws = return (ws { mind = newMind, hypIDs = newHypIDs, acqMap = freshAcqMap })
    where
      frametime   = let (Frame attrs _) = (frame ws) in frameTime attrs
      -- delete acquisitions older than 1 sec
      freshAcqMap = IDMap.filter (\acq -> (frametime - (acquisitionTime acq)) <= 1.0) (acqMap ws)
      newHypIDs   = (hypIDs ws) \\ ((IDMap.keys $ acqMap ws) \\ (IDMap.keys freshAcqMap))
      newMind     = foldl (\m h -> removeHypothesis h m) (mind ws) ((\\) (IDMap.keys $ acqMap ws) (IDMap.keys freshAcqMap))

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
          recordWorldEvent (["Constrained acquisition " ++ (show acqID) ++ ":"] ++ (map show (getConstrainers (mind ws'))), emptyElem) >>
          constrainAcquisitionExplainers' acqIDs' ws'
              where
                acq          = getItemFromMap (acqMap ws) acqID
                noiseIDs'    = keys (IDMap.filter ((==) acqID) (noiseMap ws))
                trackIDs'    = keys (IDMap.filter (\(Track acq' _ _ _) -> acq' == acq) (trackMap ws))
                hs           = IDSet.fromList (trackIDs' ++ noiseIDs')
                cID          = read $ show $ nextConstrainer ws
                cIDs         = map HasInt [cID..(cID + (IDSet.size hs))] :: [ConstrainerID]
                constrainers = [(constrainer, IDSet.delete object hs, object)
                                | (constrainer, object) <- zip cIDs (IDSet.toList hs) :: [(ConstrainerID, ObjectID)]]
                               :: [(ConstrainerID, SubjectIDs, ObjectID)]
                ws'          = ws { mind = foldl (\m (c, ss, o) -> addConstrainer c ss oneOf o m) (mind ws) constrainers }

acqDistance :: Acquisition
            -> Acquisition
            -> Double
acqDistance acq acq' = sqrt (((acquisitionX acq) - (acquisitionX acq'))^2 +
                             ((acquisitionY acq) - (acquisitionY acq'))^2)

acqDelta :: Acquisition
         -> Acquisition
         -> Double
acqDelta acq acq' = abs $ (acquisitionTime acq) - (acquisitionTime acq')