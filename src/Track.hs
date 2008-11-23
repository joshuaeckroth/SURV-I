module Track where
import Acquisition
import World
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types
import WrappedInts.IDMap (insert, foldWithKey)

hypothesizeTracks :: CategoryID
                  -> WorldState
                  -> World WorldState
hypothesizeTracks catID ws = hypothesizeTracks' catID (acqIDs ws) ws

hypothesizeTracks' :: CategoryID
                   -> [AcquisitionID]
                   -> WorldState
                   -> World WorldState
hypothesizeTracks' _     []     ws = return ws
hypothesizeTracks' catID (acqID:acqIDs) ws =
    recordTrack hypID acqID ws' >> hypothesizeTracks' catID acqIDs ws'
        where
          hypID     = 1 + (head (hypIDs ws))
          newHypIDs = [hypID] ++ (hypIDs ws)
          newTracks = insert hypID [acqID] (tracks ws)
          newMind   = addHypothesis hypID catID (const Medium) (mind ws)
          ws'       = ws { mind = newMind, hypIDs = newHypIDs, tracks = newTracks }

recordTrack :: HypothesisID -> AcquisitionID -> WorldState -> World WorldState
recordTrack hypID acqID ws =
    recordWorldEvent (["Track " ++ (show hypID) ++ " explains " ++ (show acqID)],
                      emptyElem)
                         >> return ws
