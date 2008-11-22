module Track where
import Acquisition
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types
import WrappedInts.IDMap (insert, foldWithKey)

generateTrackHypotheses :: [AcquisitionID]
                        -> AcquisitionMap
                        -> TrackMap
                        -> [HypothesisID]
                        -> CategoryID
                        -> Mind Level Level Level
                        -> (TrackMap, [HypothesisID], Mind Level Level Level)

generateTrackHypotheses []     _      tracks hs _     mind = (tracks, hs, mind)
generateTrackHypotheses (a:as) acqMap tracks hs catID mind =
    generateTrackHypotheses as acqMap newTracks newHs catID newMind
        where
          hypID     = 1 + (head hs)
          newHs     = [hypID] ++ hs
          newTracks = insert hypID [a] tracks
          newMind   = addHypothesis hypID catID (const Medium) mind


showTracks :: TrackMap -> String
showTracks tracks = foldWithKey foldTracks "" tracks

foldTracks :: HypothesisID -> [AcquisitionID] -> String -> String
foldTracks h acqIDs s = "Track " ++ (show h) ++ " explains " ++ (foldTracks' acqIDs) ++ "\n" ++ s
                        
foldTracks' []     = ""
foldTracks' (a:as) = (show a) ++ " " ++ (foldTracks' as)


