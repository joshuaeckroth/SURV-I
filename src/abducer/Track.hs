module Track where
import Detection
import World
import Types
import Reasoner.Core
import Reasoner.Types
import Reasoner.Constrainers
import Vocabulary
import WrappedInts.Types
import WrappedInts.IDMap (insert, getItemFromMap, keys, elems)
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
    hypothesizeNewTracks catID ((currentDetIDs ws) \\ (IDMap.keys intersections))
    where
      tm             = trackMap ws
      intersections  = intersectDetections (trackHeads tm) tm (currentDetIDs ws) (detMap ws) (curFrame ws)

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
      hypothesizeContinuingTracks' _     []                                       ws = return ws
      hypothesizeContinuingTracks' catID ((did,(Track det _ _ (Just ptid))):rest) ws =
          hypothesizeContinuingTracks' catID rest ws'
              where
                hypID                = nextHypID ws
                track                = Track det hypID Nothing (Just ptid)
                Track pdet _ _ pprev = getItemFromMap (trackMap ws) ptid -- recreate continued track with 'next' link
                ptrack               = Track pdet ptid (Just hypID) pprev
                explainID            = nextExplainer ws
                newHypIDs            = [hypID] ++ (hypIDs ws)
                newTrackIDs          = (trackIDs ws) ++ [hypID]
                newTrackMap          = insert hypID track $ insert ptid ptrack (trackMap ws)
                newMind              = addExplains explainID hypID did $
                                       addHypothesis hypID catID (scoreTrack (curFrame ws) hypID newTrackMap) (mind ws)
                ws'                  = addTrackRelations track $
                                       ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

-- | Hypothesize split tracks
hypothesizeSplitTracks :: CategoryID       -- ^ Hypothesis category
                       -> WorldState       -- ^ World state
                       -> World WorldState -- ^ Resulting world
hypothesizeSplitTracks catID ws =
    recordWorldEvent (["Continuing tracks:"] ++ (showTracks continuingTracks (trackMap ws'') (detMap ws'') (curFrame ws'')) ++ ["END"], emptyElem) >>
    recordWorldEvent (["Nonheads and their continuations:"] ++ (map (\(t, ts) -> (show t) ++ ": " ++ (show ts)) splits) ++ ["END"], emptyElem) >>
    recordWorldEvent (["Split \"New\" tracks for each continued track:"] ++ [(show newTracks)] ++ ["END"], emptyElem) >>
                     return ws'
    where
      tm               = trackMap ws
      continuingTracks = trackHeads' tm
      -- build a list of pairs: [(continued track, [list of track points that continue it in this frame])]
      splits           = [(nonHead,
                           filter (\tid -> let (Track det _ _ (Just tid')) = getItemFromMap tm tid in
                                           tid' == nonHead && (detProp detFrame det) == (curFrame ws))
                           continuingTracks)
                          | nonHead <- IDMap.keys tm, not $ isTrackHead nonHead tm] :: [(TrackID, [TrackID])]

      -- constrain split continuations
      ws'              = foldl (\ws hs -> addCyclicConstrainers hs constrainerOneOf ws) ws (map snd splits)

      -- generate 'new' tracks for each continued track;
      -- the track ID (set to 0 here) will be updated later
      newTracks        = [Track (trackDetection tid tm) 0 Nothing (Just nonhead)
                          | (nonhead, ts) <- splits, (length ts) > 1, tid <- ts]

      -- hypothesize the new tracks
      ws''             = foldl (hypothesizeNewSplitTrack catID) ws' newTracks
      ws'''            = foldl addSplitAdjusters ws'' (map snd splits)

hypothesizeNewSplitTrack :: CategoryID -> WorldState -> Track -> WorldState
hypothesizeNewSplitTrack catID ws track = ws''
    where
      Track det _ _ (Just p) = track
                               
      -- create duplicate of nonhead (aka p, above)
      hypID            = nextHypID ws
      explainID        = nextExplainer ws
      newHypIDs        = [hypID] ++ (hypIDs ws)
      Track pdet _ _ _ = getItemFromMap (trackMap ws) p
      pdid             = lookupDetectionID pdet (detMap ws)
      ptrack           = Track pdet hypID Nothing Nothing
      newTrackIDs      = (trackIDs ws) ++ [hypID]
      newTrackMap      = insert hypID ptrack (trackMap ws)
      newMind          = addExplains explainID hypID pdid
                         (addHypothesis hypID catID (scoreTrack (curFrame ws) hypID newTrackMap) (mind ws))
      ws'              = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

      -- create new head and link to duplicate
      hypID'           = nextHypID ws'
      explainID'       = nextExplainer ws'
      newHypIDs'       = [hypID'] ++ (hypIDs ws')
      track'           = Track det hypID' Nothing (Just hypID) -- (hypID == previous track's)
      newTrackIDs'     = (trackIDs ws') ++ [hypID']
      newTrackMap'     = insert hypID' track' newTrackMap -- add head
      did              = lookupDetectionID det (detMap ws')
      newMind'         = addExplains explainID' hypID' did
                         (addHypothesis hypID' catID (scoreTrack (curFrame ws') hypID' newTrackMap') (mind ws'))
      ws''             = addTrackRelations track' $
                         ws' { mind = newMind', hypIDs = newHypIDs', trackIDs = newTrackIDs', trackMap = newTrackMap' }

addSplitAdjusters :: WorldState -- ^ World state
                  -> [TrackID]  -- ^ Split track points
                  -> WorldState -- ^ Resulting world state
addSplitAdjusters ws tids = addSplitAdjusters' tids tids ws
    where
      addSplitAdjusters' :: [TrackID] -> [TrackID] -> WorldState -> WorldState
      addSplitAdjusters' []         _     ws = ws
      addSplitAdjusters' (tid:tids) tids' ws = addSplitAdjusters' tids tids' ws''
          where
            tm                  = trackMap ws
            competingTracks     = [] {- (tids' \\ [tid]) ++ (tracksExplainingSameDetection tid tm) -}
            complimentaryTracks = foldl (\tids tid -> tids ++ (tracksExplainingSameDetection tid tm)) [] (tids' \\ [tid])
            ws'                 = foldl (\ws ctid -> ws { mind = addAdjuster (nextAdjuster ws) tid refuteAdjuster ctid (mind ws) }) ws competingTracks
            ws''                = foldl (\ws ctid -> ws { mind = addAdjuster (nextAdjuster ws) tid increaseAdjuster ctid (mind ws) }) ws' complimentaryTracks

            refuteAdjuster      = (Right (Just $ refuteAdjuster' True, Just $ refuteAdjuster' False))
            increaseAdjuster    = (Right (Just $ increaseAdjuster' True, Just $ increaseAdjuster' False))

            -- if subject is believed, cause refutation of object; otherwise, increase likelihood of object
            refuteAdjuster' :: Bool -> Level -> Level -> Level
            refuteAdjuster' believed sconf oconf = if believed then Lowest else increaseLevel oconf
            
            -- if subject is believed, increase likelihood of object; otherwise, do nothing
            increaseAdjuster' :: Bool -> Level -> Level -> Level
            increaseAdjuster' believed sconf oconf = if believed then increaseLevel oconf else oconf

-- | Connect track heads with rest of track by way of adjusters and implications (implies & impliedBy)
addTrackRelations :: Track      -- ^ Track whose points need the relations
                  -> WorldState -- ^ World state
                  -> WorldState -- ^ Resulting world state
addTrackRelations (Track _ _   _ Nothing)     ws = ws
addTrackRelations (Track _ tid _ (Just ptid)) ws =
    case IDMap.lookup ptid (trackMap ws) of -- check if previous track point is still present
      Nothing -> ws -- no previous track point
      Just t' -> addTrackRelations t' ws'
              {-- addConstrainerType constrainerImplies tid [ptid] $ --}
              {-- addConstrainerType constrainerImpliedBy ptid [tid] ws' --}
          where
            adjuster = (Right (Just $ trackPointsAdjuster True, Just $ trackPointsAdjuster False))
            ws'      = ws { mind = addAdjuster (nextAdjuster ws) tid adjuster ptid (mind ws) }

            trackPointsAdjuster :: Bool  -- ^ More recent point believed or not believed?
                                -> Level -- ^ Confidence of more recent point 
                                -> Level -- ^ Unadjusted confidence of less recent point
                                -> Level -- ^ Adjusted confidence of less recent point
            trackPointsAdjuster believed sconf oconf = if believed then Highest else Lowest

-- | Score a track hypothesis (this is the hypothesis's a priori score)
scoreTrack :: Frame     -- ^ Current frame
           -> TrackID   -- ^ Track ID to score
           -> TrackMap  -- ^ Current track map
           -> Level     -- ^ \'Current situation\'
           -> Level     -- ^ Resulting score
scoreTrack frame tid tm l = level
    where
      track = getItemFromMap tm tid
      level = case track of
                Track det _ _ Nothing     -> Medium
                Track det _ _ (Just ptid) ->
                    case IDMap.lookup ptid tm of -- check if previous track point is still present
                      Nothing                          -> Medium -- no previous track point
                      Just (track'@(Track det' _ _ _)) -> level'
                        where
                          duration   = trackDuration track tm
                          delta      = detDelta det det'
                          areaFactor = (detProp detArea det) / (detProp detArea det')
                          level'
                              | isClosestToExpectedLocation tid tm = Highest -- closest to expected location among other track points
                                                                             -- that continue same prior track point
                              | duration > 1.0 && areaFactor <= 1.25 && areaFactor >= 0.75 = VeryHigh
                              | delta < 0.4 && duration > 0.6 && areaFactor <= 1.25 && areaFactor >= 0.75 = High
                              | delta < 0.4 = SlightlyHigh
                              | delta < 0.7 = Medium
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
            (expectedCx, expectedCy) = trackExpectedLocation track tm
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
     "\n\texpected location: " ++ (show $ trackExpectedLocation track tm) ++ ", " ++
     "\n\tis closest? " ++ (if ptid /= 0 then show $ isClosestToExpectedLocation tid tm else "N/A") ++ ", " ++
     "\n\tradius: " ++ (show $ trackRadius (trackExpectedLocation track tm) track tm) ++ ", " ++
     "\n\tdistance from expectation: " ++ (show $ trackDistanceFromExpectedLocation tid tm) ++ ", " ++ 
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
      did = lookupDetectionID det dm

instance Show Track where
    show (Track det tid ntid ptid) = "(Track " ++
                                     "(cx: " ++ (show $ detProp detCx det) ++ ", cy: " ++ (show $ detProp detCy det) ++ ")" ++
                                     " " ++ (show tid) ++ " " ++
                                     case ntid of Nothing -> "Nothing"; (Just n) -> (show n)
                                     ++ " " ++
                                     case ptid of Nothing -> "Nothing"; (Just p) -> (show p)
                                     ++ ")"

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
                                         trackExpectedLocation track tm
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

-- | Remove a whole track (following \'previous\' links)
removeTrack :: Track -> TrackMap -> TrackMap
removeTrack (Track _ tid _ Nothing)     tm = IDMap.delete tid tm
removeTrack (Track _ tid _ (Just ptid)) tm = IDMap.delete tid $
                                             if IDMap.member ptid tm then -- check if previous track point is still present
                                                 removeTrack (getItemFromMap tm ptid) tm
                                             else tm -- no previous track point

-- | Update \'next\' and \'previous\' links in all tracks
--
-- Tracks points may have been removed if the associated detections have been removed
-- from the function 'updateDetections' or a track's head has been removed because
-- it was deemed refuted.
updateLinks :: TrackMap -> TrackMap
updateLinks tm = updateMissingHeads $ foldl (\tm tid -> updateLink tid 0 tm) tm (trackHeads' tm)

-- IS THIS still relevant?
updateMissingHeads :: TrackMap -> TrackMap
updateMissingHeads tm = foldl (\tm tid -> updateMissingHead tid tm) tm (IDMap.keys tm)

updateMissingHead :: TrackID -> TrackMap -> TrackMap
updateMissingHead tid tm = case getItemFromMap tm tid of
                             (Track det tid Nothing      ptid) -> tm
                             (Track det tid (Just ntid') ptid) -> if (IDMap.member ntid' tm) then
                                                                      insert tid (Track det tid (Just ntid') ptid) tm
                                                                  else
                                                                      insert tid (Track det tid Nothing ptid) tm

-- | Update a single \'next\' link
updateLink :: TrackID  -- ^ The new "next" link
           -> TrackID  -- ^ Track to change
           -> TrackMap
           -> TrackMap
-- initial call: second argument is 0, so get "previous" track from this head
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
    recordWorldEvent ([""], recordWorldEventInFrame frame $ tracksToXml tids tm frame) >>
    return ws
    where
      tids  = Data.List.intersect (IDSet.toList $ acceptedHypotheses (mind ws)) (keys (trackMap ws))
      tm    = trackMap ws
      dm    = detMap ws
      frame = curFrame ws

-- | Finds track points explaining the same detection as the given track point.
--
-- Resulting list does not include the given track point.
tracksExplainingSameDetection :: TrackID
                              -> TrackMap
                              -> [TrackID]
tracksExplainingSameDetection tid tm = filter (\tid' -> let Track det' _ _ _ = getItemFromMap tm tid' in det' == det) ((IDMap.keys tm) \\ [tid])
    where
      Track det _ _ _ = getItemFromMap tm tid

-- | Construct a list of track heads
--
-- Here, track heads are any tracks without a next track point (so this includes new tracks).
trackHeads :: TrackMap  -- ^ Current track map
           -> [TrackID] -- ^ Track heads
trackHeads tm = IDMap.keys $ IDMap.filter (\(Track _ _ ntid _) -> case ntid of
                                                                    Nothing -> True
                                                                    Just _  -> False) tm

-- | Construct a list of head tracks that are not new tracks, i.e., whose \'previous\' link is not Nothing
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

trackExpectedLocation :: Track
                      -> TrackMap
                      -> (Double, Double)
trackExpectedLocation       (Track det _ _ Nothing)     _  = (detProp detCx det,
                                                              detProp detCy det)
trackExpectedLocation track@(Track det _ _ (Just ptid)) tm =
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
                | cx2 == cx1 && cy2 < cy1 = (cx2, cy2 - dist) -- downward; angle = NaN
                | cx2 == cx1 && cy2 > cy1 = (cx2, cy2 + dist) -- upward
                | cy2 == cy1 && cx2 < cx1 = (cx2 - dist, cy2) -- right
                | cy2 == cy2 && cx2 > cx1 = (cx2 + dist, cy2) -- left
                | cx2 > cx1  && cy2 < cy1 = (cx2 + dist * (cos angle), cy2 - dist * (sin angle)) -- down-left
                | cx2 > cx1  && cy2 > cy1 = (cx2 + dist * (cos angle), cy2 + dist * (sin angle)) -- up-left
                | cx2 < cx1  && cy2 < cy1 = (cx2 - dist * (cos angle), cy2 - dist * (abs $ sin angle)) -- down-right
                | cx2 < cx1  && cy2 > cy1 = (cx2 - dist * (cos angle), cy2 + dist * (abs $ sin angle)) -- up-right
                | otherwise = (cx2, cy2) -- track points overlap (detections overlap)

trackDistanceFromExpectedLocation :: TrackID -> TrackMap -> Double
trackDistanceFromExpectedLocation tid tm = let (Track det _ _ p) = getItemFromMap tm tid in
                                           case p of Nothing   -> 0.0
                                                     Just ptid ->
                                                         let track' = getItemFromMap tm ptid
                                                             (expectedCx, expectedCy) = trackExpectedLocation track' tm
                                                         in sqrt (((detProp detCx det) - expectedCx)^2 +
                                                                  ((detProp detCy det) - expectedCy)^2)

isClosestToExpectedLocation :: TrackID -> TrackMap -> Bool
isClosestToExpectedLocation tid tm = foldl (\b tid' -> b && (dist < trackDistanceFromExpectedLocation tid' tm)) True tids
    where
      dist = trackDistanceFromExpectedLocation tid tm
      tids = trackAlternativeContinuations tid tm

-- | Find track points that have the same previous track point as the track given in the first parameter
trackAlternativeContinuations :: TrackID -> TrackMap -> [TrackID]
trackAlternativeContinuations tid tm = let (Track _ _ _ p) = getItemFromMap tm tid in
                                       case p of Nothing   -> []
                                                 Just ptid -> filter (\tid' -> let (Track _ _ _ p'') = getItemFromMap tm tid' in
                                                                               case p'' of Nothing -> False; Just ptid' -> ptid' == ptid)
                                                              ((IDMap.keys tm) \\ [tid])

-- | Radius of expected location (see 'trackExpectedLocation')
trackRadius :: (Double, Double) -> Track -> TrackMap -> Double
trackRadius (ecx, ecy) track@(Track det _ _ _) tm
    | speed == 0.0 = 30.0
    | speed < 20.0 = min (10 * expDist) $ 5 * speed
    | otherwise    = min (10 * expDist) $ 60.0
    where
      speed    = trackSpeed track tm
      (cx,cy)  = (detProp detCx det, detProp detCy det)
      expDist  = sqrt ((cx - ecx)^2 + (cy - ecy)^2)

trackDetection :: TrackID -> TrackMap -> Detection
trackDetection tid tm = det
    where
      Track det _ _ _ = getItemFromMap tm tid