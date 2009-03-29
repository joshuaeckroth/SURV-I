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
import qualified Data.List
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
      dids           = currentDetIDs ws
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
    recordWorldEvent (["Split \"New\" tracks for each continued track:"] ++ [(show newTracks)] ++ ["END"], emptyElem) >>
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
                              addSplitAdjusters hypID ts $
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
addTrackImplications (Track _ tid _ (Just ptid)) ws =
    case IDMap.lookup ptid (trackMap ws) of -- check if previous track point is still present
      Just t' -> addTrackImplications t' ws'
      Nothing -> ws -- no previous track point
    where
      newMind = addConstrainer (nextConstrainer ws) (IDSet.singleton ptid) implies tid $
                addConstrainer (1 + (nextConstrainer ws)) (IDSet.singleton tid) impliedBy ptid (mind ws)
      ws'     = ws { mind = newMind }

-- | Score a track hypothesis (this is the hypothesis's a prior score)
scoreTrack :: Frame    -- ^ Current frame
           -> TrackID  -- ^ Track ID to score
           -> TrackMap -- ^ Current track map
           -> Level    -- ^ \'Current situation\'
           -> Level    -- ^ Resulting score
scoreTrack frame tid tm l = level
    where
      track = getItemFromMap tm tid
      level = case track of
                Track det _ _ Nothing     -> Medium
                Track det _ _ (Just ptid) ->
                    case IDMap.lookup ptid tm of -- check if previous track point is still present
                      Nothing                          -> SlightlyLow -- no previous track point
                      Just (track'@(Track det' _ _ _)) -> level'
                        where
                          (expectedCx, expectedCy) = trackExpectedLocation frame track' tm
                          expectedDist             = sqrt (((detProp detCx det) - expectedCx)^2 +
                                                           ((detProp detCy det) - expectedCy)^2)
                          duration                 = trackDuration track tm
                          delta                    = detDelta det det'
                          areaFactor               = (detProp detArea det) / (detProp detArea det')
                                                      
                          level'
                              | duration > 1.0 && expectedDist < 5.0 && areaFactor <= 1.25 && areaFactor >= 0.75 = VeryHigh
                              | delta < 0.7 && (duration > 0.6 || expectedDist < 10.0) && areaFactor <= 1.25 && areaFactor >= 0.75 = High
                              | delta < 0.7 && expectedDist < 20.0 = SlightlyHigh
                              | delta < 0.7 && expectedDist < 30.0 = Medium
                              | otherwise = Low

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
          if dist <= radius && dist /= 0 then
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
     "\n\tradius: " ++ (show $ trackRadius (trackExpectedLocation frame track tm) track tm) ++ ", " ++
     (if ptid == 0 then "" else
          (if (IDMap.member ptid tm) then (let track' = getItemFromMap tm ptid in ("\n\tdistance from expectation: " ++ (show $ (let (ecx,ecy) = (trackExpectedLocation frame track' tm) in sqrt ((ecx - (detProp detCx det))^2 + (ecy - (detProp detCy det))^2))) ++ ", ")) else "")) ++
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
                                       Just ptid' ->
                                           case IDMap.lookup ptid' tm of -- check if previous track point is still present
                                             Just track' -> track'
                                             Nothing     -> track -- no previous track point
            ntid'                    = case ntid of
                                       Nothing    -> 0
                                       Just ntid' -> ntid'
            thisFrame                = (frameProp frameTime frame) == (frameProp frameTime $ detProp detFrame det)
            (expectedCx, expectedCy) = if (frameProp frameTime $ detProp detFrame det) /= (frameProp frameTime frame) then
                                         (detProp detCx det, detProp detCy det)
                                       else
                                         trackExpectedLocation frame track tm
            radius                   = trackRadius (expectedCx, expectedCy) track tm

-- | Keep only track hypotheses considered \'irrefutable\', \'accepted\', or \'considering\', as well as newer tracks (see 'removeOldTracks')
updateTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
updateTracks ws =
    recordWorldEvent (["Removed tracks:"] ++ (showTracks ((trackIDs ws) \\ newTrackIDs) (trackMap ws) (detMap ws) (curFrame ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, trackIDs = newTrackIDs, trackMap = newTrackMap })
    where
      m              = mind ws
      badHs          = IDSet.toList $ refutedHypotheses m

      freshTrackMap  = updateLinks (removeOldTracks (curFrame ws) (trackHeads (trackMap ws)) (trackMap ws))
      freshTrackMap' = updateLinks (foldl (\tm h -> IDMap.delete h tm) freshTrackMap badHs)

      newTrackMap    = freshTrackMap'
      newTrackIDs    = filter (\h -> elem h $ IDMap.keys newTrackMap) (trackIDs ws)
      newMind        = foldl (\m h -> removeHypothesis h m) (mind ws) ((trackIDs ws) \\ newTrackIDs)

-- Filter out tracks whose head is older than 2 seconds
removeOldTracks :: Frame -> [TrackID] -> TrackMap -> TrackMap
removeOldTracks _ []         tm = tm
removeOldTracks f (tid:tids) tm = if (trackDelta f (getItemFromMap tm tid)) < 2.0 then -- track is 'fresh'
                                      removeOldTracks f tids tm
                                  else -- remove entire track
                                      removeOldTracks f tids $ removeTrack (getItemFromMap tm tid) tm

-- | Remove a whole track (following 'previous' links)
removeTrack :: Track -> TrackMap -> TrackMap
removeTrack (Track _ tid _ Nothing)     tm = IDMap.delete tid tm
removeTrack (Track _ tid _ (Just ptid)) tm = IDMap.delete tid $
                                             if IDMap.member ptid tm then -- check if previous track point is still present
                                                 removeTrack (getItemFromMap tm ptid) tm
                                             else tm -- no previous track point

-- | Update \'next\' and \'prev\' links in all tracks
--
-- Tracks points may have been removed if the associated detections have been removed
-- from the function 'updateDetections' or a track's head has been removed because
-- it was deemed refuted.
updateLinks :: TrackMap -> TrackMap
updateLinks tm = updateMissingHeads $ foldl (\tm tid -> updateLink tid 0 tm) tm (trackHeads' tm)

updateMissingHeads :: TrackMap -> TrackMap
updateMissingHeads tm = foldl (\tm tid -> updateMissingHead tid tm) tm (IDMap.keys tm)

updateMissingHead :: TrackID -> TrackMap -> TrackMap
updateMissingHead tid tm = case getItemFromMap tm tid of
                             (Track det tid Nothing      ptid) -> tm
                             (Track det tid (Just ntid') ptid) -> if (IDMap.member ntid' tm) then
                                                                      insert tid (Track det tid (Just ntid') ptid) tm
                                                                  else
                                                                      insert tid (Track det tid Nothing ptid) tm

-- | Update a single \'new\' link
updateLink :: TrackID  -- ^ The new "next" link
           -> TrackID  -- ^ Track to change
           -> TrackMap
           -> TrackMap
-- initial call: second argument is 0, so get "prev" track from this head
updateLink ntid 0   tm = updateLink ntid ptid tm
    where Track _ _ _ (Just ptid) = getItemFromMap tm ntid
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
-- Here, track heads are any tracks without a next track point (so this includes new tracks).
trackHeads :: TrackMap  -- ^ Current track map
           -> [TrackID] -- ^ Track heads
trackHeads tm = IDMap.keys $ IDMap.filter (\(Track _ _ ntid _) -> case ntid of
                                                                    Nothing -> True
                                                                    Just _  -> False) tm

-- | Construct a list of head tracks that are not new tracks, i.e., whose prevID does not equal 0
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
trackDuration (Track det _ _ Nothing)     _  = 0.0 -- no previous track point
trackDuration (Track det _ _ (Just ptid)) tm =
    case IDMap.lookup ptid tm of -- check if previous track point is still present
      Just (ptrack@(Track det' _ _ _)) -> (frameProp frameTime $ detProp detFrame det) -
                                          (frameProp frameTime $ detProp detFrame det') +
                                         (trackDuration ptrack tm)
      Nothing                          -> 0.0 -- no previous track point

-- | Track speed
--
-- Speed of track with most recent detection acq and second most recent detection det\' =
-- (detDist det det\') (detDelta det det\')
--
-- The maximum possible speed is 50 (forced limitation)
trackSpeed :: Track
           -> TrackMap
           -> Double
trackSpeed (Track det _ _ Nothing)     _  = 0.0 -- no previous track point
trackSpeed (Track det _ _ (Just ptid)) tm =
    case IDMap.lookup ptid tm of -- check if previous track point is still present
      Just (Track det' _ _ _) -> min 50 $ (detDist det det') / (detDelta det det')
      Nothing                 -> 0.0 -- no previous track point

-- | Overall track distance since origin
trackDistance :: Track
              -> TrackMap
              -> Double
trackDistance (Track det _ _ Nothing)     _  = 0.0
trackDistance (Track det _ _ (Just ptid)) tm =
    case IDMap.lookup ptid tm of -- check if previous track point is still present
      Just (ptrack@(Track det' _ _ _)) -> (detDist det det') + (trackDistance ptrack tm)
      Nothing                          -> 0.0 -- no previous track point

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
trackExpectedLocation frame track@(Track det _ _ (Just ptid)) tm =
    case IDMap.lookup ptid tm of -- check if previous track point is still present
      Nothing                 -> (detProp detCx det, detProp detCy det)
      Just (Track det' _ _ _) -> (cx3, cy3)
          where
            (cx1, cy1) = (detProp detCx det', detProp detCy det') -- old position
            (cx2, cy2) = (detProp detCx det,  detProp detCy det)  -- current position
            speed      = trackSpeed track tm
            delta      = 0.333 -- look one frame into the future
            dist       = speed * delta -- expected distance
            angle      = atan ((cy2 - cy1) / (cx2 - cx1))
            (cx3, cy3)
                | cx2 == cx1 = (cx2, dist + cy2) -- angle = NaN
                | otherwise  = (dist * (cos angle) + cx2, dist * (sin angle) + cy2)

-- | Radius of expected location (see 'trackExpectedLocation')
trackRadius :: (Double, Double) -> Track -> TrackMap -> Double
trackRadius (ecx, ecy) track@(Track det _ _ _) tm
    | speed == 0.0 = 30.0
    | speed < 20.0 = min (5 * expDist) $ 3 * speed
    | otherwise    = min (5 * expDist) $ 60.0
    where
      speed    = trackSpeed track tm
      (cx,cy)  = (detProp detCx det, detProp detCy det)
      expDist  = sqrt ((cx - ecx)^2 + (cy - ecy)^2)

trackDetection :: TrackID -> TrackMap -> Detection
trackDetection tid tm = det
    where
      Track det _ _ _ = getItemFromMap tm tid