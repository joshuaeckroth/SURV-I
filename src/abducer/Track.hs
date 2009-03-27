module Track where
import Detection
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
import Text.Printf (printf)

-- | Hypothesize and score tracks, both new and continuing
hypothesizeTracks :: CategoryID       -- ^ Hypothesis category
                  -> WorldState       -- ^ World state
                  -> World WorldState -- ^ Resulting world
hypothesizeTracks catID ws =
    -- we want to hypothesize continuing tracks before new so that continuing tracks
    -- don't pick up hypothesized new tracks and continue them
    hypothesizeContinuingTracks catID intersections ws >>=
    hypothesizeNewTracks catID (dids \\ intersectingAs)
    where
      frame          = curFrame ws
      tm             = trackMap ws
      dm             = detMap ws
      ts             = keys tm
      dids           = detIDs ws
      intersections  = intersectDetections (trackHeads tm) tm dids dm frame
      intersectingAs = IDMap.keys intersections

-- | Hypothesize and score one new track per detection
hypothesizeNewTracks :: CategoryID       -- ^ Hypothesis category
                     -> [DetectionID]    -- ^ Detections requiring new track hypotheses
                     -> WorldState       -- ^ World state
                     -> World WorldState -- ^ Resulting world
hypothesizeNewTracks _     []         ws = return ws
hypothesizeNewTracks catID (did:dids) ws = hypothesizeNewTracks catID dids ws'
    where
      hypID       = nextHypID ws
      explainID   = nextExplainer ws
      newHypIDs   = [hypID] ++ (hypIDs ws)
      det         = getItemFromMap (detMap ws) did
      track       = Track det hypID Nothing Nothing
      newTrackIDs = (trackIDs ws) ++ [hypID]
      newTrackMap = insert hypID track (trackMap ws)
      newMind     = addExplains explainID hypID did
                    (addHypothesis hypID catID (scoreTrack (curFrame ws) hypID newTrackMap) (mind ws))
      ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

-- | Hypothesize and score continuing tracks nearby each detection
hypothesizeContinuingTracks :: CategoryID              -- ^ Hypothesis category
                            -> HypothesisMap [TrackID] -- ^ Detection-Track intersections
                            -> WorldState              -- ^ World state
                            -> World WorldState        -- ^ Resulting world
hypothesizeContinuingTracks catID intersections ws
    | IDMap.null intersections = return ws
    | otherwise                = hypothesizeContinuingTracks' catID detTrackPairs ws
    where
      detTrackPairs = [(did, track) |
                       did   <- IDMap.keys intersections,
                       track <- (constructContinuingTracks
                                 (getItemFromMap (detMap ws) did)    -- detection
                                 (map (getItemFromMap (trackMap ws)) -- tracks that intersect
                                  (getItemFromMap intersections did)))]

      -- The tracks created here have a TrackID of 0; this is modified in hypothesizeContinuingTracks'
      constructContinuingTracks :: Detection
                                -> [Track]
                                -> [Track]
      constructContinuingTracks det []                     = []
      constructContinuingTracks det ((Track _ tid _ _):ts) = [Track det 0 Nothing (Just tid)] ++
                                                             constructContinuingTracks det ts

      hypothesizeContinuingTracks' :: CategoryID
                                   -> [(DetectionID, Track)]
                                   -> WorldState
                                   -> World WorldState
      hypothesizeContinuingTracks' _     []                                             ws = return ws
      hypothesizeContinuingTracks' catID ((did,track@(Track det _ n (Just prev))):rest) ws =
          hypothesizeContinuingTracks' catID rest ws'
              where
                hypID            = nextHypID ws
                track'           = Track det hypID n (Just prev)
                explainID        = nextExplainer ws
                newHypIDs        = [hypID] ++ (hypIDs ws)
                newTrackIDs      = (trackIDs ws) ++ [hypID]

                -- get the "continued" track
                Track det' _ _ p = getItemFromMap (trackMap ws) prev

                -- recreate the "continued" track with a "next" link and
                -- insert both the new track and the updated "continued" track
                newTrackMap      = insert hypID track' $ insert prev (Track det' prev (Just hypID) p) (trackMap ws)

                newMind          = addExplains explainID hypID did $
                                   addHypothesis hypID catID (scoreTrack (curFrame ws) hypID newTrackMap) (mind ws)
                ws'              = addTrackImplications track' $
                                   ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

-- | Hypothesize split tracks
hypothesizeSplitTracks :: CategoryID       -- ^ Hypothesis category
                       -> WorldState       -- ^ World state
                       -> World WorldState -- ^ Resulting world
hypothesizeSplitTracks catID ws =
    recordWorldEvent (["Continuing tracks:"] ++ (showTracks continuingTracks (trackMap ws') (detMap ws') (curFrame ws')) ++ ["END"], emptyElem) >>
    recordWorldEvent (["Nonheads and their continuations:"] ++ (map (\(t, ts) -> (show t) ++ ": " ++ (show ts)) splits) ++ ["END"], emptyElem) >>
    recordWorldEvent (["\"New\" tracks for each continued track:"] ++ [(show newTracks)] ++ ["END"], emptyElem) >>
                     return ws'
    where
      tm               = trackMap ws
      continuingTracks = trackHeads' tm
      -- build a list of pairs: [(continued track, [list of track points that continue it])]
      splits           = [(nonHead,
                           filter (\tid -> let (Track _ _ _ (Just tid')) = getItemFromMap tm tid in
                                           tid' == nonHead)
                           continuingTracks)
                          | nonHead <- IDMap.keys tm, not $ isTrackHead nonHead tm] :: [(TrackID, [TrackID])]
      -- generate 'new' tracks for each continued track;
      -- the result is a list of pairs: [(new track, [trackIDs that it "replaces"])]
      -- the track IDs (set to 0 here) will be updated later
      newTracks        = [(Track (trackDetection (head ts) tm) 0 Nothing Nothing, ts)
                          | (_, ts) <- splits, (length ts) > 1]

      -- hypothesize the new tracks
      ws'              = foldl hypothesizeNewSplitTrack ws newTracks

      hypothesizeNewSplitTrack :: WorldState -> (Track, [TrackID]) -> WorldState
      hypothesizeNewSplitTrack ws (track, ts) = ws'
          where
            hypID           = nextHypID ws
            explainID       = nextExplainer ws
            newHypIDs       = [hypID] ++ (hypIDs ws)
            Track det _ n p = track
            track'          = Track det hypID n p
            newTrackIDs     = (trackIDs ws) ++ [hypID]
            newTrackMap     = insert hypID track' (trackMap ws)
            did             = foldWithKey (\k e d -> if e == det then k else d) 0 (detMap ws)
            newMind         = addExplains explainID hypID did
                              (addHypothesis hypID catID (scoreTrack (curFrame ws) hypID newTrackMap) (mind ws))
            ws'             = constrainSplitTracks ([hypID] ++ ts) $
                              -- addSplitAdjusters hypID ts $
                              ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

      addSplitAdjusters :: TrackID -> [TrackID] -> WorldState -> WorldState
      addSplitAdjusters _   []          ws = ws
      addSplitAdjusters tid (tid':tids') ws = addSplitAdjusters tid tids' ws'
          where
            newMind = addAdjuster (nextAdjuster ws) tid (Right (Just $ adjuster True, Just $ adjuster False)) tid' (mind ws)
            ws'     = ws { mind = newMind }

            adjuster :: Bool  -- ^ True if "new" track is accepted, False if "new" track is refuted
                     -> Level -- ^ Confidence of the "new" track
                     -> Level -- ^ Confidence of the "continued" track
                     -> Level -- ^ Adjusted confidence of the "continued" track
            adjuster True  confNew confContinuing = Lowest -- if new track is accepted, ensure continuing tracks are refuted
            adjuster False confNew confContinuing = increaseLevel confContinuing -- if new track is rejected, increase confidence

      constrainSplitTracks :: [TrackID] -> WorldState -> WorldState
      constrainSplitTracks tids ws = ws'
          where
            cID          = read $ show $ nextConstrainer ws
            cIDs         = map HasInt [cID..(cID + (length tids))] :: [ConstrainerID]
            constrainers = [(constrainer, IDSet.fromList (tids \\ [object]), object)
                            | (constrainer, object) <- zip cIDs tids :: [(ConstrainerID, ObjectID)]]
                           :: [(ConstrainerID, SubjectIDs, ObjectID)]
            ws'          = ws { mind = foldl (\m (c, ss, o) -> addConstrainer c ss oneOf o m) (mind ws) constrainers }

-- | Connect track heads with rest of track by way of track implications (implies & impliedBy)
addTrackImplications :: Track      -- ^ Track to connect by way of implications
                     -> WorldState -- ^ World state
                     -> WorldState -- ^ Resulting world state
addTrackImplications (Track _ _   _ Nothing)     ws = ws
addTrackImplications (Track _ tid _ (Just tid')) ws = addTrackImplications t' ws'
    where
      t'      = getItemFromMap (trackMap ws) tid'
      newMind = addConstrainer (nextConstrainer ws) (IDSet.singleton tid') implies tid $
                addConstrainer (1 + (nextConstrainer ws)) (IDSet.singleton tid) impliedBy tid' (mind ws)
      ws'     = ws { mind = newMind }

-- | Score a track hypothesis (this is the hypothesis's a prior score)
scoreTrack :: Frame    -- ^ Current frame
           -> TrackID  -- ^ Track ID to score
           -> TrackMap -- ^ Current track map
           -> Level    -- ^ "Current situation"
           -> Level    -- ^ Resulting score
scoreTrack frame tid tm l = level
    where
      track = getItemFromMap tm tid
      level = case track of
                Track det _ _ Nothing     -> Medium
                Track det _ _ (Just tid') -> level'
                    where
                      track'@(Track det' _ _ _) = getItemFromMap tm tid'
                      
                      (expectedCx, expectedCy)  = trackExpectedLocation frame track' tm
                      expectedDist              = sqrt (((detProp detCx det) - expectedCx)^2 +
                                                        ((detProp detCy det) - expectedCy)^2)
                      duration                  = trackDuration track tm
                      delta                     = detDelta det det'

                      level'
                         | duration > 1.0 || expectedDist < 10.0 = Highest
                         | delta < 0.4 && (duration > 0.6 || expectedDist < 20.0) = VeryHigh
                         | delta < 0.4 && expectedDist < 30.0 = High
                         | delta < 0.4 = SlightlyHigh
                         | expectedDist < 100.0 = Medium
                         | otherwise    = Lowest

-- | Find all detections that \'intersect\' the given tracks
intersectDetections :: [TrackID]               -- ^ Tracks to intersect
                    -> TrackMap                -- ^ Current track map
                    -> [DetectionID]           -- ^ Detections to intersect
                    -> DetectionMap            -- ^ Detection map
                    -> Frame                   -- ^ Current frame
                    -> HypothesisMap [TrackID] -- ^ Resulting intersecting detections
intersectDetections tids tm dids dm f = intersectDetections' tids tm dids dids dm f
    where
      intersectDetections' :: [TrackID]
                           -> TrackMap
                           -> [DetectionID]
                           -> [DetectionID]
                           -> DetectionMap
                           -> Frame
                           -> HypothesisMap [TrackID]
      intersectDetections' []     _         _       _  _       _ = IDMap.empty
      intersectDetections' (tid:tids) tm []         dids' dm f = intersectDetections' tids tm dids' dids' dm f
      intersectDetections' (tid:tids) tm (did:dids) dids' dm f =
          if dist <= radius && dist /= 0 && delta <= time then
              -- track 'intersects' detection
              -- add the track ID to the map
              IDMap.insertWithKey (\det tids tids' -> tids ++ tids') did [tid]
                       (intersectDetections' (tid:tids) tm dids dids' dm f)
          else
              intersectDetections' (tid:tids) tm dids dids' dm f
          where
            track                    = getItemFromMap tm tid
            (expectedCx, expectedCy) = trackExpectedLocation f track tm
            det                      = getItemFromMap dm did
            cx                       = detProp detCx det
            cy                       = detProp detCy det
            dist                     = sqrt ((expectedCx - cx)^2 + (expectedCy - cy)^2)
            radius                   = trackRadius (expectedCx, expectedCy) track tm
            delta                    = trackDelta f track
            time                     = 0.7 -- seconds

-- | Human format of track log
showTracks :: [TrackID]    -- ^ Track IDs to log
           -> TrackMap     -- ^ Current track map
           -> DetectionMap -- ^ Current detection map
           -> Frame        -- ^ Current frame
           -> [String]     -- ^ Human log
showTracks []                  _        _       _ = []
showTracks (tid:tids) tm dm frame =
    ["Track point " ++ (show tid) ++
     " [detection: " ++ (show did) ++ ", " ++
     " cx: " ++ (show $ detProp detCx det) ++ ", " ++
     " cy: " ++ (show $ detProp detCy det) ++ ", " ++
     "\n\tprevious track point: " ++ (show ptid) ++ ", " ++
     "\n\tnext track point: " ++ (show ntid) ++ ", " ++
     "\n\tduration: " ++ (printf "%.2f" $ trackDuration track tm) ++ " secs, " ++
     "\n\tspeed: " ++ (printf "%.2f" $ trackSpeed track tm) ++ ", " ++
     "\n\tdistance: " ++ (printf "%.2f" $ trackDistance track tm) ++ ", " ++
     "\n\texpected location: " ++ (show $ trackExpectedLocation frame track tm) ++ ", " ++
     "\n\tscore: " ++ (show $ scoreTrack frame tid tm Medium) ++ "]"]
    ++ showTracks tids tm dm frame
    where
      track             = getItemFromMap tm tid
      (det, ntid, ptid) = -- track's detection, next track ID, prev track ID
          case track of
            Track det _ (Just ntid) (Just ptid) -> (det, ntid, ptid)
            Track det _ (Just ntid) Nothing     -> (det, ntid, 0)
            Track det _ Nothing     (Just ptid) -> (det, 0, ptid)
            Track det _ Nothing     Nothing     -> (det, 0, 0)
      did = foldWithKey (\k e d -> if e == det then k else d) 0 dm

instance Show Track where
    show (Track det tid ntid ptid) = "Track " ++ (show tid)

-- | XML format of track log
tracksToXml :: [TrackID]          -- ^ Track IDs to log
            -> TrackMap           -- ^ Current track map
            -> Frame              -- ^ Current frame
            -> [HaXml.Content ()] -- ^ XML log
tracksToXml []                 _         _ = []
tracksToXml (tid:tids) tm frame =
    trackToXml track ++ tracksToXml tids tm frame
    where
      track = getItemFromMap tm tid
      trackToXml (Track det _ ntid ptid) =
          [worldElem "Track" [("id", show tid),
                              ("camera", detProp detCamera det),
                              ("cx", show $ detProp detCx det),
                              ("cy", show $ detProp detCy det),
                              ("ocx", show $ detProp detCx det'),
                              ("ocy", show $ detProp detCy det'),
                              ("prevID", show ptid'),
                              ("nextID", show ntid'),
                              ("ecx", show expectedCx),
                              ("ecy", show expectedCy),
                              ("radius", show radius),
                              ("thisFrame", show thisFrame)] []]
          where
            ptid'                    = case ptid of
                                       Nothing    -> 0
                                       Just ptid' -> ptid'
            Track det' _ _ _         = case ptid of
                                       Nothing    -> track
                                       Just ptid' -> getItemFromMap tm ptid'
            ntid'                    = case ntid of
                                       Nothing    -> 0
                                       Just ntid' -> ntid'
            thisFrame                = (frameProp frameTime frame) == (frameProp frameTime $ detProp detFrame det)
            (expectedCx, expectedCy) = if (frameProp frameTime $ detProp detFrame det) /= (frameProp frameTime frame) then
                                         (detProp detCx det, detProp detCy det)
                                       else
                                         trackExpectedLocation frame track tm
            radius                   = trackRadius (expectedCx, expectedCy) track tm

-- | Keep only track hypotheses considered \'irrefutable\' or \'accepted\'
updateTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
updateTracks ws =
    recordWorldEvent (["Removed tracks:"] ++ (showTracks ((trackIDs ws) \\ newTrackIDs) (trackMap ws) (detMap ws) (curFrame ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap })
    where
      m              = mind ws
      frametime      = frameProp frameTime (curFrame ws)
      badHs          = IDSet.toList $ refutedHypotheses m
      goodHs         = IDSet.toList $ IDSet.union (irrefutableHypotheses m)
                       (IDSet.union (acceptedHypotheses m)
                             (IDSet.union (consideringHypotheses m) (unacceptableHypotheses m)))

      -- filter out refuted track heads and tracks whose head is older than 1 sec
      freshTrackMap  = foldl (\tm h -> removeTrack (getItemFromMap tm h) tm)
                       (trackMap ws)
                       (IDMap.keys (IDMap.filterWithKey (\h _ -> (elem h badHs) && (isTrackHead h (trackMap ws))) (trackMap ws)))
      freshTrackMap' = removeOldTracks (curFrame ws) (trackHeads freshTrackMap) freshTrackMap

      newTrackMap    = updateLinks freshTrackMap'
      newTrackIDs    = filter (\n -> elem n $ IDMap.keys newTrackMap) (trackIDs ws)
      newMind        = foldl (\m h -> removeHypothesis h m) (mind ws) ((trackIDs ws) \\ newTrackIDs)
      newHypIDs      = (IDMap.keys newTrackMap) ++ (IDMap.keys (noiseMap ws)) ++ (IDMap.keys (detMap ws))

      removeOldTracks :: Frame -> [TrackID] -> TrackMap -> TrackMap
      removeOldTracks _ []         tm = tm
      removeOldTracks f (tid:tids) tm = if (trackDelta f (getItemFromMap tm tid)) <= 1.0 then -- track is 'fresh'
                                            removeOldTracks f tids tm
                                        else -- remove entire track
                                            removeOldTracks f tids $ removeTrack (getItemFromMap tm tid) tm

      -- remove a whole track (following 'previous' links)
      removeTrack :: Track -> TrackMap -> TrackMap
      removeTrack (Track _ tid _ Nothing)     tm = IDMap.delete tid tm
      removeTrack (Track _ tid _ (Just ptid)) tm = IDMap.delete tid $
                                                   if IDMap.member ptid tm then
                                                       removeTrack (getItemFromMap tm ptid) tm
                                                   else tm

      -- update "next" and "prev" links in all tracks (since some track points may have been removed)
      updateLinks :: TrackMap -> TrackMap
      updateLinks tm = foldl (\tm tid -> updateLink tid 0 tm) tm (trackHeads' tm)

      updateLink :: TrackID  -- ^ The new "next" link
                 -> TrackID  -- ^ Track to change
                 -> TrackMap
                 -> TrackMap
      -- initial call: second argument is 0, so get "prev" track from this head
      updateLink ntid 0   tm = updateLink ntid tid tm
          where Track _ _ _ (Just tid) = getItemFromMap tm ntid
      updateLink ntid tid tm =
          if IDMap.member tid tm then
              let Track det _ _ ptid = getItemFromMap tm tid in
              insert tid (Track det tid (Just ntid) ptid) $
                     case ptid of
                       Nothing   -> tm
                       Just ptid -> updateLink tid ptid tm
          else -- rewrite "next" track as having no previous track
              let Track det _ ntid' _ = getItemFromMap tm ntid
              in insert ntid (Track det ntid ntid' Nothing) tm

-- | Record recent track hypotheses
recordTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
recordTracks ws =
    recordWorldEvent (showTracks tids tm dm frame,
                      recordWorldEventInFrame frame $ tracksToXml tids tm frame) >>
    return ws
    where
      tids  = keys (trackMap ws)
      tm    = trackMap ws
      dm    = detMap ws
      frame = curFrame ws

-- | Construct a list of track heads
--
-- Here, track heads are any tracks without a previous track point (so this includes new tracks).
trackHeads :: TrackMap  -- ^ Current track map
           -> [TrackID] -- ^ Track heads
trackHeads tm = IDMap.keys $ IDMap.filter (\(Track _ _ ntid _) -> case ntid of
                                                                    Nothing -> True
                                                                    Just _  -> False) tm

-- | Construct a list of head tracks that are not new tracks
trackHeads' :: TrackMap -> [TrackID]
trackHeads' tm = IDMap.keys $ IDMap.filter (\(Track _ _ ntid ptid) -> case (ntid,ptid) of
                                                                        (Nothing, Just _) -> True
                                                                        otherwise         -> False) tm

isTrackHead :: TrackID
            -> TrackMap
            -> Bool
isTrackHead tid tm = answer
    where
      (Track _ _ ntid _) = getItemFromMap tm tid
      answer             = ntid == Nothing

-- | Find the head of a track
trackHead :: Track
          -> TrackMap
          -> Track
trackHead track@(Track _ _ Nothing     _) _  = track
trackHead       (Track _ _ (Just ntid) _) tm = trackHead (getItemFromMap tm ntid) tm

-- | Overall duration of track since origin
trackDuration :: Track  -- ^ Track of interest
              -> TrackMap
              -> Double -- ^ Track duration
trackDuration (Track det _ _ Nothing)     _  = 0.0 -- no previous track
trackDuration (Track det _ _ (Just ptid)) tm = (frameProp frameTime $ detProp detFrame det) -
                                               (frameProp frameTime $ detProp detFrame det') +
                                               (trackDuration ptrack tm)
    where
      ptrack@(Track det' _ _ _) = getItemFromMap tm ptid

-- | Track speed
--
-- Speed of track with most recent detection acq and second most recent detection det\' =
-- (detDist det det\') (detDelta det det\')
--
-- The maximum possible speed is 100 (forced limitation)
trackSpeed :: Track
           -> TrackMap
           -> Double
trackSpeed (Track det _ _ Nothing)     _  = 0.0
trackSpeed (Track det _ _ (Just ptid)) tm = min 100 $ (detDist det det') / (detDelta det det')
    where
      (Track det' _ _ _) = getItemFromMap tm ptid

-- | Overall track distance since origin
trackDistance :: Track
              -> TrackMap
              -> Double
trackDistance (Track det _ _ Nothing)     _  = 0.0
trackDistance (Track det _ _ (Just ptid)) tm = (detDist det det') + (trackDistance ptrack tm)
    where
      ptrack@(Track det' _ _ _) = getItemFromMap tm ptid

trackDelta :: Frame
           -> Track
           -> Double
trackDelta frame (Track det _ _ _) = (frameProp frameTime frame) -
                                     (frameProp frameTime $ detProp detFrame det)

trackExpectedLocation :: Frame
                      -> Track
                      -> TrackMap
                      -> (Double, Double)
trackExpectedLocation frame       (Track det _ _ Nothing)     _  = (detProp detCx det,
                                                                    detProp detCy det)
trackExpectedLocation frame track@(Track det _ _ (Just ptid)) tm = (cx3, cy3)
    where
      (Track det' _ _ _) = getItemFromMap tm ptid
      (cx1, cy1)         = (detProp detCx det', detProp detCy det') -- old position
      (cx2, cy2)         = (detProp detCx det,  detProp detCy det)  -- current position
      speed              = trackSpeed track tm
      delta              = (trackDelta frame track) + 0.333 -- add one frame's time
      dist               = speed * delta -- expected distance
      m                  = (cy1 - cy2) / (cx2 - cx1) -- cy1 - cy2 due to coordinate system
      (cx3, cy3) 
          | (cy1 - cy2) == 0 = if (cx2 - cx1) > 0 then -- moved right
                                   (cx2 + dist, cy2)
                               else                  -- moved left
                                   (cx2 - dist, cy2)
          | (cx2 - cx1) == 0 = if (cy1 - cy2) > 0 then -- moved up
                                   (cx2, cy2 - dist)
                               else                  -- moved down
                                   (cx2, cy2 + dist)
          | otherwise        = let cx3 = if (cx2 - cx1) > 0 then -- moved right
                                             dist / ((sqrt 2) * m) + cx2
                                         else                  -- moved left
                                             (0 - dist / ((sqrt 2) * m)) + cx2
                               in (cx3, cy2 - m * (cx3 - cx2))

trackRadius :: (Double, Double) -> Track -> TrackMap -> Double
trackRadius (ecx, ecy) track@(Track det _ _ _) tm
    | speed == 0.0 = 75.0
    | speed < 50.0 = min (3 * expDist) $ 3 * speed
    | otherwise    = min (3 * expDist) $ 150.0
    where
      speed    = trackSpeed track tm
      (cx,cy)  = (detProp detCx det, detProp detCy det)
      expDist  = sqrt ((cx - ecx)^2 + (cy - ecy)^2)

trackDetection :: TrackID -> TrackMap -> Detection
trackDetection tid tm = det
    where
      Track det _ _ _ = getItemFromMap tm tid