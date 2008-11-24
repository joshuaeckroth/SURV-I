module Track where
import Acquisition
import World
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types
import WrappedInts.IDMap (insert, foldWithKey, getItemFromMap)
import Text.XML.HaXml.Types as HaXml

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
    hypothesizeTracks' catID acqIDs ws'
        where
          hypID       = 1 + (head (hypIDs ws))
          explainID   = nextID explainers (mind ws)
          newHypIDs   = [hypID] ++ (hypIDs ws)
          acq         = getItemFromMap (acqMap ws) acqID
          track       = Track acq []
          newTrackMap = insert hypID track (trackMap ws)
          newMind     = addExplains explainID hypID acqID
                        (addHypothesis hypID catID (scoreTrack track newTrackMap) (mind ws))
          ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackMap = newTrackMap }

scoreTrack :: Track -> TrackMap -> Level -> Level
scoreTrack t trackMap' _ = Medium

showTrack :: [Track] -> TrackMap -> [String]
showTrack []     _         = []
showTrack (t:ts) trackMap' = [""]

trackToXml :: [Track] -> TrackMap -> [HaXml.Content]
trackToXml []     _         = []
trackToXml (t:ts) trackMap' =
    [worldElem "Track" [] []]
    ++ trackToXml ts trackMap'

updateTracks :: WorldState -> World WorldState
updateTracks ws = return ws

recordTracks :: WorldState -> World WorldState
recordTracks ws = return ws

trackDuration :: Track -> Double
trackDuration (Track acq ts)
    | null ts   = 0.0 -- no previous track
    | otherwise = let (Track acq2 _) = head ts in
                  (acquisitionTime acq) - (acquisitionTime acq2) + (trackDuration $ head ts)
