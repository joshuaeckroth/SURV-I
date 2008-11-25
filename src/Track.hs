module Track where
import Acquisition
import World
import Types
import Reasoner.Core
import Reasoner.Types
import Reasoner.Constrainers
import Vocabulary
import WrappedInts.Types
import WrappedInts.IDMap (insert, foldWithKey, getItemFromMap, keys, elems)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import Data.List ((\\))
import Text.XML.HaXml.Types as HaXml

hypothesizeTracks :: CategoryID
                  -> WorldState
                  -> World WorldState
hypothesizeTracks catID ws =
    hypothesizeNewTracks catID nonIntersectingAs ws >>=
    hypothesizeContinuingTracks catID intersectingAs
    where
      trackMap'         = trackMap ws
      acqMap'           = acqMap ws
      ts                = keys trackMap'
      as                = acqIDs ws
      intersectingAs    = IDSet.toList (intersectAcquisitions ts trackMap' as as acqMap')
      nonIntersectingAs = as \\ intersectingAs

hypothesizeNewTracks :: CategoryID
                   -> [AcquisitionID]
                   -> WorldState
                   -> World WorldState
hypothesizeNewTracks _     []             ws = return ws
hypothesizeNewTracks catID (acqID:acqIDs) ws =
    hypothesizeNewTracks catID acqIDs ws'
        where
          hypID       = 1 + (head (hypIDs ws))
          explainID   = nextID explainers (mind ws)
          newHypIDs   = [hypID] ++ (hypIDs ws)
          acq         = getItemFromMap (acqMap ws) acqID
          track       = Track acq Nothing
          newTrackIDs = (trackIDs ws) ++ [hypID]
          newTrackMap = insert hypID track (trackMap ws)
          newMind     = addExplains explainID hypID acqID
                        (addHypothesis hypID catID (scoreTrack hypID newTrackMap) (mind ws))
          ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

hypothesizeContinuingTracks :: CategoryID -> [AcquisitionID] -> WorldState -> World WorldState
hypothesizeContinuingTracks _     []             ws = return ws
hypothesizeContinuingTracks catID (acqID:acqIDs) ws =
    hypothesizeContinuingTracks' catID tracks acqID ws >>=
    constrainContinuingTracks tracks acqID >>=
    hypothesizeContinuingTracks catID acqIDs
        where
          tracks = intersectTracks (getItemFromMap (acqMap ws) acqID) (elems (trackMap ws))

hypothesizeContinuingTracks' :: CategoryID -> [Track] -> AcquisitionID -> WorldState -> World WorldState
hypothesizeContinuingTracks' _     []     _     ws = return ws
hypothesizeContinuingTracks' catID (t:ts) acqID ws =
    hypothesizeContinuingTracks' catID ts acqID ws'
        where
          hypID       = 1 + (head (hypIDs ws))
          explainID   = nextID explainers (mind ws)
          newHypIDs   = [hypID] ++ (hypIDs ws)
          acq         = getItemFromMap (acqMap ws) acqID
          track       = Track acq (Just t)
          newTrackIDs = (trackIDs ws) ++ [hypID]
          newTrackMap = insert hypID track (trackMap ws)
          newMind     = addExplains explainID hypID acqID
                        (addHypothesis hypID catID (scoreTrack hypID newTrackMap) (mind ws))
          ws'         = if duplicateTrack track (trackMap ws) then
                            ws
                        else 
                            ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

duplicateTrack :: Track -> TrackMap -> Bool
duplicateTrack (Track acq Nothing)  trackMap' = if IDMap.null (IDMap.filter (\(Track acq' _) -> acq' == acq) trackMap')
                                                then False
                                                else True
duplicateTrack (Track acq (Just t)) trackMap' = if IDMap.null (IDMap.filter (\(Track acq' _) -> acq' == acq) trackMap')
                                                then False
                                                else duplicateTrack t trackMap'

constrainContinuingTracks :: [Track] -> AcquisitionID -> WorldState -> World WorldState
constrainContinuingTracks ts acqID ws = constrainContinuingTracks' trackIDs trackIDs ws
    where
      acq      = getItemFromMap (acqMap ws) acqID
      trackIDs = keys (IDMap.filter (\(Track acq' _) -> acq == acq') (trackMap ws)) -- tracks explaining this acq
                 ++
                 keys (IDMap.filter (\acqID' -> acqID' == acqID) (noiseMap ws)) -- noise explaining this acq

constrainContinuingTracks' :: [TrackID] -> [TrackID] -> WorldState -> World WorldState
constrainContinuingTracks' []      _  ws = return ws
constrainContinuingTracks' (t:ts') ts ws = constrainContinuingTracks' ts' ts ws'
    where
      ws' = ws { mind = (addConstrainer (nextConstrainer ws) (IDSet.fromList ts) oneOf t (mind ws)) }

scoreTrack :: TrackID -> TrackMap -> Level -> Level
scoreTrack t trackMap' _ = Medium

intersectAcquisitions :: [TrackID] -> TrackMap -> [AcquisitionID] -> [AcquisitionID] -> AcquisitionMap -> AcquisitionIDs
intersectAcquisitions []     _         _       _  _       = IDSet.empty
intersectAcquisitions (t:ts) trackMap' []      as acqMap' = intersectAcquisitions ts trackMap' as as acqMap'
intersectAcquisitions (t:ts) trackMap' (a:as') as acqMap' =
    if dist <= radius && dist /= 0 && delta <= time then
        IDSet.insert a $ intersectAcquisitions (t:ts) trackMap' as' as acqMap'
    else
        intersectAcquisitions (t:ts) trackMap' as' as acqMap'
    where
      (Track trackAcq _) = getItemFromMap trackMap' t
      trackX             = acquisitionX trackAcq
      trackY             = acquisitionY trackAcq
      acq                = getItemFromMap acqMap' a
      acqX               = acquisitionX acq
      acqY               = acquisitionY acq
      dist               = sqrt((trackX - acqX)^2 + (trackY - acqY)^2)
      radius             = 25.0
      trackTime          = acquisitionTime trackAcq
      acqTime            = acquisitionTime acq
      delta              = abs (trackTime - acqTime)
      time               = 1.0 -- seconds

intersectTracks :: Acquisition -> [Track] -> [Track]
intersectTracks _   []     = []
intersectTracks acq (t:ts) =
    if dist <= radius && dist /= 0 && delta <= time then
        [t] ++ intersectTracks acq ts
    else
        intersectTracks acq ts
    where
      (Track trackAcq _) = t
      trackX             = acquisitionX trackAcq
      trackY             = acquisitionY trackAcq
      acqX               = acquisitionX acq
      acqY               = acquisitionY acq
      dist               = sqrt((trackX - acqX)^2 + (trackY - acqY)^2)
      radius             = 25.0
      trackTime          = acquisitionTime trackAcq
      acqTime            = acquisitionTime acq
      delta              = abs (trackTime - acqTime)
      time               = 1.0 -- seconds

showTracks :: [TrackID] -> TrackMap -> AcquisitionMap -> [String]
showTracks []                  _        _       = []
showTracks (trackID:trackIDs) trackMap' acqMap' =
    ["Track point " ++ (show trackID) ++
     " [acquisition: " ++ (show acqID) ++ ", " ++
     "previous track point: " ++ (show t) ++ "]"]
    ++ showTracks trackIDs trackMap' acqMap'
    where
      (acq, t) =
          case (getItemFromMap trackMap' trackID) of
            Track acq (Just t) -> (acq, foldWithKey (\k e a -> if e == t then k else a) 0 trackMap')
            Track acq Nothing  -> (acq, 0)
      acqID = foldWithKey (\k e a -> if e == acq then k else a) 0 acqMap'

tracksToXml :: [TrackID] -> TrackMap -> [HaXml.Content]
tracksToXml []                 _         = []
tracksToXml (trackID:trackIDs) trackMap' =
    tracksToXml' t ++ tracksToXml trackIDs trackMap'
    where
      t = getItemFromMap trackMap' trackID
      tracksToXml' (Track _ Nothing) = []
      tracksToXml' (Track acq (Just (Track acq2 _))) =
          [worldElem "Track" [("id", show trackID),
                              ("x1", show $ acquisitionX acq),
                              ("y1", show $ acquisitionY acq),
                              ("x2", show $ acquisitionX acq2),
                              ("y2", show $ acquisitionY acq2),
                              ("nextID", show trackID2)] []]
          where
            trackID2 = foldWithKey (\k e a -> if e == t then k else a) 0 trackMap'

updateTracks :: WorldState -> World WorldState
updateTracks ws = return ws

recordTracks :: WorldState -> World WorldState
recordTracks ws =
    recordWorldEvent (showTracks ts trackMap' acqMap',
                      recordWorldEventInFrame framenum frametime $ tracksToXml ts trackMap') >>
    return ws
    where
      ts              = trackIDs ws
      trackMap'       = trackMap ws
      acqMap'         = acqMap ws
      (Frame attrs _) = frame ws
      framenum        = show $ frameNumber attrs
      frametime       = show $ frameTime attrs

trackDuration :: Track -> Double
trackDuration (Track acq Nothing)  = 0.0 -- no previous track
trackDuration (Track acq (Just t)) = let (Track acq2 _) = t in
                                     (acquisitionTime acq) - (acquisitionTime acq2) + (trackDuration t)
