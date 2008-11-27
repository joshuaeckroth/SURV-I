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

-- | Hypothesize and score tracks, both new and continuing
hypothesizeTracks :: CategoryID       -- ^ Hypothesis category
                  -> WorldState       -- ^ World state
                  -> World WorldState -- ^ Resulting world
hypothesizeTracks catID ws =
    -- we want to hypothesize continuing tracks before new so that continuing tracks
    -- don't pick up hypothesized new tracks and continue them
    hypothesizeContinuingTracks catID intersectingAs ws >>=
    hypothesizeNewTracks catID as
    where
      trackMap'      = trackMap ws
      acqMap'        = acqMap ws
      ts             = keys trackMap'
      as             = acqIDs ws
      intersectingAs = IDSet.toList $! (intersectAcquisitions ts trackMap' as acqMap')

-- | Hypothesize and score one new track per acquisition
hypothesizeNewTracks :: CategoryID       -- ^ Hypothesis category
                     -> [AcquisitionID]  -- ^ Acquisitions requiring new track hypotheses
                     -> WorldState       -- ^ World state
                     -> World WorldState -- ^ Resulting world
hypothesizeNewTracks _     []             ws = return ws
hypothesizeNewTracks catID (acqID:acqIDs) ws =
    hypothesizeNewTracks catID acqIDs ws'
        where
          hypID       = nextHypID ws
          explainID   = nextExplainer ws
          newHypIDs   = [hypID] ++ (hypIDs ws)
          acq         = getItemFromMap (acqMap ws) acqID
          track       = Track acq Nothing
          newTrackIDs = (trackIDs ws) ++ [hypID]
          newTrackMap = insert hypID track (trackMap ws)
          newMind     = addExplains explainID hypID acqID
                        (addHypothesis hypID catID (scoreTrack hypID newTrackMap) (mind ws))
          ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

-- | Hypothesize and score continuing tracks nearby each acquisition
hypothesizeContinuingTracks :: CategoryID       -- ^ Hypothesis category
                            -> [AcquisitionID]  -- ^ Acquisitions requiring continuing track hypotheses
                            -> WorldState       -- ^ World state
                            -> World WorldState -- ^ Resulting world
hypothesizeContinuingTracks _     []             ws = return ws
hypothesizeContinuingTracks catID acqIDs' ws =
    hypothesizeContinuingTracks' catID trackAcqPairs ws
        where
          trackAcqPairs = [(track, acqID) | acqID <- acqIDs',
                                                     track <- (constructContinuingTracks
                                                               (getItemFromMap (acqMap ws) acqID)) $!
                                                              (intersectTracks (getItemFromMap (acqMap ws) acqID) (elems (trackMap ws))) ]

          constructContinuingTracks :: Acquisition
                                    -> [Track]
                                    -> [Track]
          constructContinuingTracks acq []     = []
          constructContinuingTracks acq (t:ts) = [Track acq (Just t)] ++ constructContinuingTracks acq ts

          hypothesizeContinuingTracks' :: CategoryID
                                       -> [(Track, AcquisitionID)]
                                       -> WorldState
                                       -> World WorldState
          hypothesizeContinuingTracks' _     []                   ws = return ws
          hypothesizeContinuingTracks' catID ((track,acqID):rest) ws =
              hypothesizeContinuingTracks' catID rest ws'
                  where
                    hypID       = nextHypID ws
                    explainID   = nextExplainer ws
                    newHypIDs   = [hypID] ++ (hypIDs ws)
                    newTrackIDs = (trackIDs ws) ++ [hypID]
                    newTrackMap = insert hypID track (trackMap ws)
                    newMind     = addExplains explainID hypID acqID
                                  (addHypothesis hypID catID (scoreTrack hypID newTrackMap) (mind ws))
                    ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs,
                                       trackMap = newTrackMap }

-- | Score a track hypothesis
scoreTrack :: TrackID -> TrackMap -> Level -> Level
scoreTrack t trackMap' _ = level
    where
      track = getItemFromMap trackMap' t
      level = case track of
                Track acq Nothing               -> Medium
                Track acq (Just (Track acq2 _)) -> level'
                    where
                      delta = abs ((acquisitionTime acq) - (acquisitionTime acq2))
                      dist  = sqrt (((acquisitionX acq) - (acquisitionX acq2))^2 +
                                    ((acquisitionY acq) - (acquisitionY acq2))^2)
                      level'
                         | delta < 0.4 && dist < 15.0  = Highest
                         | delta < 0.4 && dist < 25.0  = VeryHigh
                         | delta < 0.7 && dist < 50.0  = High
                         | delta < 0.7 && dist < 75.0  = SlightlyHigh
                         | delta < 0.7 && dist < 125.0 = Medium
                         | delta < 0.7 && dist < 150.0 = SlightlyLow
                         | delta < 0.7 && dist < 175.0 = Low
                         | delta < 0.7 && dist < 200.0 = VeryLow
                         | otherwise                   = Lowest

-- | Find all acquisitions that \'intersect\' the given tracks
intersectAcquisitions :: [TrackID]       -- ^ Tracks to intersect
                      -> TrackMap        -- ^ Current track map
                      -> [AcquisitionID] -- ^ Acquisitions to intersect
                      -> AcquisitionMap  -- ^ Acquisition map
                      -> AcquisitionIDs  -- ^ Resulting intersecting acquisitions
intersectAcquisitions ts trackMap' as acqMap' = intersectAcquisitions' ts trackMap' as as acqMap'
    where
      intersectAcquisitions' []     _         _       _  _       = IDSet.empty
      intersectAcquisitions' (t:ts) trackMap' []      as acqMap' = intersectAcquisitions' ts trackMap' as as acqMap'
      intersectAcquisitions' (t:ts) trackMap' (a:as') as acqMap' =
          if dist <= radius && dist /= 0 && delta <= time then
              (IDSet.insert a) $! (intersectAcquisitions' (t:ts) trackMap' as' as acqMap')
          else
              intersectAcquisitions' (t:ts) trackMap' as' as acqMap'
          where
            (Track trackAcq _) = getItemFromMap trackMap' t
            trackX             = acquisitionX trackAcq
            trackY             = acquisitionY trackAcq
            acq                = getItemFromMap acqMap' a
            acqX               = acquisitionX acq
            acqY               = acquisitionY acq
            dist               = sqrt((trackX - acqX)^2 + (trackY - acqY)^2)
            radius             = 200.0
            trackTime          = acquisitionTime trackAcq
            acqTime            = acquisitionTime acq
            delta              = abs (trackTime - acqTime)
            time               = 0.5 -- seconds

-- | Find all tracks that \'intersect\' the given acquisition
intersectTracks :: Acquisition -- ^ Acquisition to intersect
                -> [Track]     -- ^ Tracks to check
                -> [Track]     -- ^ Intersecting tracks
intersectTracks _   []     = []
intersectTracks acq (t:ts) =
    if dist <= radius && dist /= 0 && delta <= time then
        ([t] ++) $! (intersectTracks acq ts)
    else
        intersectTracks acq ts
    where
      (Track trackAcq _) = t
      trackX             = acquisitionX trackAcq
      trackY             = acquisitionY trackAcq
      acqX               = acquisitionX acq
      acqY               = acquisitionY acq
      dist               = sqrt((trackX - acqX)^2 + (trackY - acqY)^2)
      radius             = 200.0
      trackTime          = acquisitionTime trackAcq
      acqTime            = acquisitionTime acq
      delta              = abs (trackTime - acqTime)
      time               = 0.5 -- seconds

-- | Human format of track log
showTracks :: [TrackID]      -- ^ Track IDs to log
           -> TrackMap       -- ^ Current track map
           -> AcquisitionMap -- ^ Current acquisition map
           -> [String]       -- ^ Human log
showTracks []                  _        _       = []
showTracks (trackID:trackIDs) trackMap' acqMap' =
    ["Track point " ++ (show trackID) ++
     " [acquisition: " ++ (show acqID) ++ ", " ++
     "previous track point: " ++ (show t') ++ ", " ++
     "duration: " ++ (show $ trackDuration t) ++ " secs, " ++
     "score: " ++ (show $ scoreTrack trackID trackMap' Medium) ++ "]"]
    ++ showTracks trackIDs trackMap' acqMap'
    where
      t         = getItemFromMap trackMap' trackID
      (acq, t') =
          case t of
            Track acq (Just t') -> (acq, foldWithKey (\k e a -> if e == t' then k else a) 0 trackMap')
            Track acq Nothing   -> (acq, 0)
      acqID = foldWithKey (\k e a -> if e == acq then k else a) 0 acqMap'

-- | XML format of track log
tracksToXml :: [TrackID]       -- ^ Track IDs to log
            -> TrackMap        -- ^ Current track map
            -> [HaXml.Content] -- ^ XML log
tracksToXml []                 _         = []
tracksToXml (trackID:trackIDs) trackMap' =
    tracksToXml' t ++ tracksToXml trackIDs trackMap'
    where
      t = getItemFromMap trackMap' trackID
      tracksToXml' (Track _ Nothing) = []
      tracksToXml' (Track acq (Just t'@(Track acq2 _))) =
          [worldElem "Track" [("id", show trackID),
                              ("x1", show $ acquisitionX acq),
                              ("y1", show $ acquisitionY acq),
                              ("x2", show $ acquisitionX acq2),
                              ("y2", show $ acquisitionY acq2),
                              ("prevID", show trackID2)] []]
          where
            trackID2 = foldWithKey (\k e a -> if e == t' then k else a) 0 trackMap'

-- | Keep only track hypotheses considered \'irrefutable\' or \'accepted\'
updateTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
updateTracks ws =
    recordWorldEvent (["Removed tracks:"] ++ (showTracks ((trackIDs ws) \\ newTrackIDs) (trackMap ws) (acqMap ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, trackIDs = newTrackIDs, trackMap = newTrackMap })
    where
      m             = mind ws
      frametime     = let (Frame attrs _) = (frame ws) in frameTime attrs
      goodHs        = IDSet.toList $ IDSet.union (irrefutableHypotheses m) (IDSet.union (acceptedHypotheses m) (consideringHypotheses m))
      -- filter out tracks older than 5 sec
      freshTrackMap = IDMap.filter (\(Track acq _) -> (frametime - (acquisitionTime acq)) < 5.0) (trackMap ws)
      newTrackMap   = IDMap.filterWithKey (\h _ -> elem h goodHs) freshTrackMap
      newTrackIDs   = filter (\n -> elem n $ IDMap.keys newTrackMap) (trackIDs ws)
      newMind       = foldl (\m h -> removeHypothesis h m) (mind ws) ((\\) (IDMap.keys $ trackMap ws) (IDMap.keys newTrackMap))

-- | Record recent track hypotheses
recordTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
recordTracks ws =
    recordWorldEvent (showTracks ts trackMap' acqMap',
                      recordWorldEventInFrame framenum frametime $ tracksToXml ts trackMap') >>
    return ws
    where
--      ts              = trackIDs ws
      ts              = keys (trackMap ws)
      trackMap'       = trackMap ws
      acqMap'         = acqMap ws
      (Frame attrs _) = frame ws
      framenum        = show $ frameNumber attrs
      frametime       = show $ frameTime attrs

-- | Measure how long ago a track began
trackDuration :: Track  -- ^ Track of interest
              -> Double -- ^ Track duration
trackDuration (Track acq Nothing)  = 0.0 -- no previous track
trackDuration (Track acq (Just t)) = let (Track acq2 _) = t in
                                     (acquisitionTime acq) - (acquisitionTime acq2) + (trackDuration t)
