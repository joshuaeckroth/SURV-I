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
hypothesizeTracks' catID (a:as) ws =
    hypothesizeTracks' catID as ws'
        where
          hypID     = 1 + (head (hypIDs ws))
          newHypIDs = [hypID] ++ (hypIDs ws)
          newTracks = insert hypID [a] (tracks ws)
          newMind   = addHypothesis hypID catID (const Medium) (mind ws)
          ws'       = ws { mind = newMind, hypIDs = newHypIDs, tracks = newTracks }

showTracks :: TrackMap -> String
showTracks tracks = foldWithKey foldTracks "" tracks

foldTracks :: HypothesisID -> [AcquisitionID] -> String -> String
foldTracks h acqIDs s = "Track " ++ (show h) ++ " explains " ++ (foldTracks' acqIDs) ++ "\n" ++ s
                        
foldTracks' []     = ""
foldTracks' (a:as) = (show a) ++ " " ++ (foldTracks' as)


