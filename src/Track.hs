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
import Text.Printf (printf)

-- | Hypothesize and score tracks, both new and continuing
hypothesizeTracks :: CategoryID       -- ^ Hypothesis category
                  -> WorldState       -- ^ World state
                  -> World WorldState -- ^ Resulting world
hypothesizeTracks catID ws =
    -- we want to hypothesize continuing tracks before new so that continuing tracks
    -- don't pick up hypothesized new tracks and continue them
    hypothesizeContinuingTracks catID intersections ws >>=
    hypothesizeNewTracks catID (as \\ intersectingAs)
    where
      f              = frame ws
      trackMap'      = trackMap ws
      acqMap'        = acqMap ws
      ts             = keys trackMap'
      as             = acqIDs ws
      intersections  = intersectAcquisitions (trackHeads trackMap') trackMap' as acqMap' f
      intersectingAs = IDMap.keys intersections

-- | Hypothesize and score one new track per acquisition
hypothesizeNewTracks :: CategoryID       -- ^ Hypothesis category
                     -> [AcquisitionID]  -- ^ Acquisitions requiring new track hypotheses
                     -> WorldState       -- ^ World state
                     -> World WorldState -- ^ Resulting world
hypothesizeNewTracks _     []             ws = return ws
hypothesizeNewTracks catID (acqID:acqIDs) ws = hypothesizeNewTracks catID acqIDs ws'
    where
      hypID       = nextHypID ws
      explainID   = nextExplainer ws
      newHypIDs   = [hypID] ++ (hypIDs ws)
      acq         = getItemFromMap (acqMap ws) acqID
      track       = Track acq hypID Nothing Nothing
      newTrackIDs = (trackIDs ws) ++ [hypID]
      newTrackMap = insert hypID track (trackMap ws)
      newMind     = addExplains explainID hypID acqID
                    (addHypothesis hypID catID (scoreTrack (frame ws) hypID newTrackMap) (mind ws))
      ws'         = ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

-- | Hypothesize and score continuing tracks nearby each acquisition
hypothesizeContinuingTracks :: CategoryID              -- ^ Hypothesis category
                            -> HypothesisMap [TrackID] -- ^ Acquisition-Track intersections
                            -> WorldState              -- ^ World state
                            -> World WorldState        -- ^ Resulting world
hypothesizeContinuingTracks catID intersections ws
    | IDMap.null intersections = return ws
    | otherwise                = hypothesizeContinuingTracks' catID acqTrackPairs ws
    where
      acqTrackPairs = [(acqID, track) |
                       acqID <- IDMap.keys intersections,
                       track <- (constructContinuingTracks
                                 (getItemFromMap (acqMap ws) acqID)  -- acquisition
                                 (map (getItemFromMap (trackMap ws)) -- tracks that intersect
                                  (getItemFromMap intersections acqID)))]

      -- The tracks created here have a TrackID of 0; this is modified in hypothesizeContinuingTracks'
      constructContinuingTracks :: Acquisition
                                -> [Track]
                                -> [Track]
      constructContinuingTracks acq []                         = []
      constructContinuingTracks acq ((Track _ trackID _ _):ts) = [Track acq 0 Nothing (Just trackID)] ++
                                                                 constructContinuingTracks acq ts

      hypothesizeContinuingTracks' :: CategoryID
                                   -> [(AcquisitionID, Track)]
                                   -> WorldState
                                   -> World WorldState
      hypothesizeContinuingTracks' _     []                                     ws = return ws
      hypothesizeContinuingTracks' catID ((acqID,track@(Track acq _ n (Just prev))):rest) ws =
          hypothesizeContinuingTracks' catID rest ws'
              where
                hypID            = nextHypID ws
                track'           = Track acq hypID n (Just prev)
                explainID        = nextExplainer ws
                newHypIDs        = [hypID] ++ (hypIDs ws)
                newTrackIDs      = (trackIDs ws) ++ [hypID]

                -- get the "continued" track
                Track acq' _ _ p = getItemFromMap (trackMap ws) prev

                -- recreate the "continued" track with a "next" link and
                -- insert both the new track and the updated "continued" track
                newTrackMap      = insert hypID track' $ insert prev (Track acq' prev (Just hypID) p) (trackMap ws)

                newMind          = addExplains explainID hypID acqID $
                                   addHypothesis hypID catID (scoreTrack (frame ws) hypID newTrackMap) (mind ws)
                ws'              = addTrackImplications track' $
                                   ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

hypothesizeSplitTracks :: CategoryID
                       -> WorldState
                       -> World WorldState
hypothesizeSplitTracks catID ws =
    recordWorldEvent (["Continuing tracks:"] ++ (showTracks continuingTracks (trackMap ws') (acqMap ws') (frame ws')) ++ ["END"], emptyElem) >>
    recordWorldEvent (["Nonheads and their continuations:"] ++ (map (\(t, ts) -> (show t) ++ ": " ++ (show ts)) splits) ++ ["END"], emptyElem) >>
    recordWorldEvent (["\"New\" tracks for each continued track:"] ++ [(show newTracks)] ++ ["END"], emptyElem) >>
                     return ws'
    where
      trackMap'        = trackMap ws
      continuingTracks = trackHeads' trackMap'
      -- build a list of pairs: [(continued track, [list of track points that continue it])]
      splits           = [(nonHead,
                           filter (\trackID -> let (Track _ _ _ (Just trackID')) = getItemFromMap trackMap' trackID in
                                               trackID' == nonHead)
                           continuingTracks)
                          | nonHead <- IDMap.keys trackMap', not $ isTrackHead nonHead trackMap'] :: [(TrackID, [TrackID])]
      -- generate 'new' tracks for each continued track;
      -- the result is a list of pairs: [(new track, [trackIDs that it "replaces"])]
      -- the track IDs (set to 0 here) will be updated later
      newTracks        = [(Track (trackAcquisition (head ts) trackMap') 0 Nothing Nothing, ts)
                          | (_, ts) <- splits, (length ts) > 1]

      -- hypothesize the new tracks
      ws'              = foldl hypothesizeNewSplitTrack ws newTracks

      hypothesizeNewSplitTrack :: WorldState -> (Track, [TrackID]) -> WorldState
      hypothesizeNewSplitTrack ws (track, ts) = ws'
          where
            hypID           = nextHypID ws
            explainID       = nextExplainer ws
            newHypIDs       = [hypID] ++ (hypIDs ws)
            Track acq _ n p = track
            track'          = Track acq hypID n p
            newTrackIDs     = (trackIDs ws) ++ [hypID]
            newTrackMap     = insert hypID track' (trackMap ws)
            acqID           = foldWithKey (\k e a -> if e == acq then k else a) 0 (acqMap ws)
            newMind         = addExplains explainID hypID acqID
                              (addHypothesis hypID catID (scoreTrack (frame ws) hypID newTrackMap) (mind ws))
            ws'             = constrainSplitTracks ([hypID] ++ ts) $
                              -- addSplitAdjusters hypID ts $
                              ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap }

      addSplitAdjusters :: TrackID -> [TrackID] -> WorldState -> WorldState
      addSplitAdjusters _       []                  ws = ws
      addSplitAdjusters trackID (trackID':trackIDs) ws = addSplitAdjusters trackID trackIDs ws'
          where
            newMind = addAdjuster (nextAdjuster ws) trackID (Right (Just $ adjuster True, Just $ adjuster False)) trackID' (mind ws)
            ws'     = ws { mind = newMind }

            adjuster :: Bool  -- ^ True if "new" track is accepted, False if "new" track is refuted
                     -> Level -- ^ Confidence of the "new" track
                     -> Level -- ^ Confidence of the "continued" track
                     -> Level -- ^ Adjusted confidence of the "continued" track
            adjuster True  confNew confContinuing = Lowest -- if new track is accepted, ensure continuing tracks are refuted
            adjuster False confNew confContinuing = increaseLevel confContinuing -- if new track is rejected, increase confidence

      constrainSplitTracks :: [TrackID] -> WorldState -> WorldState
      constrainSplitTracks trackIDs' ws = ws'
          where
            cID          = read $ show $ nextConstrainer ws
            cIDs         = map HasInt [cID..(cID + (length trackIDs'))] :: [ConstrainerID]
            constrainers = [(constrainer, IDSet.fromList (trackIDs' \\ [object]), object)
                            | (constrainer, object) <- zip cIDs trackIDs' :: [(ConstrainerID, ObjectID)]]
                           :: [(ConstrainerID, SubjectIDs, ObjectID)]
            ws'          = ws { mind = foldl (\m (c, ss, o) -> addConstrainer c ss oneOf o m) (mind ws) constrainers }

addTrackImplications :: Track
                     -> WorldState
                     -> WorldState
addTrackImplications (Track _ _       _ Nothing)         ws = ws
addTrackImplications (Track _ trackID _ (Just trackID')) ws = addTrackImplications t' ws'
    where
      t'      = getItemFromMap (trackMap ws) trackID'
      newMind = addConstrainer (nextConstrainer ws) (IDSet.singleton trackID') implies trackID $
                addConstrainer (1 + (nextConstrainer ws)) (IDSet.singleton trackID) impliedBy trackID' (mind ws)
      ws'     = ws { mind = newMind }

-- | Score a track hypothesis
scoreTrack :: Frame -> TrackID -> TrackMap -> Level -> Level
scoreTrack f t trackMap' l = level
    where
      track = getItemFromMap trackMap' t
      level = case track of
                Track acq _ _ Nothing         -> Medium
                Track acq _ _ (Just trackID') -> level'
                    where
                      track'@(Track acq' _ _ _) = getItemFromMap trackMap' trackID'
                      
                      (expectedX, expectedY)    = trackExpectedLocation f track' trackMap'
                      expectedDist              = sqrt (((acquisitionX acq) - expectedX)^2 +
                                                        ((acquisitionY acq) - expectedY)^2)
                      duration                  = trackDuration track trackMap'
                      delta                     = (acquisitionTime acq) - (acquisitionTime acq')

                      level'
                         | duration > 1.0 || expectedDist < 10.0 = Highest
                         | delta < 0.4 && (duration > 0.6 || expectedDist < 20.0) = VeryHigh
                         | delta < 0.4 && expectedDist < 30.0 = High
                         | delta < 0.4 = SlightlyHigh
                         | expectedDist < 100.0 = Medium
                         | otherwise    = Lowest

-- | Find all acquisitions that \'intersect\' the given tracks
intersectAcquisitions :: [TrackID]               -- ^ Tracks to intersect
                      -> TrackMap                -- ^ Current track map
                      -> [AcquisitionID]         -- ^ Acquisitions to intersect
                      -> AcquisitionMap          -- ^ Acquisition map
                      -> Frame                   -- ^ Current frame
                      -> HypothesisMap [TrackID] -- ^ Resulting intersecting acquisitions
intersectAcquisitions ts trackMap' as acqMap' f = intersectAcquisitions' ts trackMap' as as acqMap' f
    where
      intersectAcquisitions' :: [TrackID]
                             -> TrackMap
                             -> [AcquisitionID]
                             -> [AcquisitionID]
                             -> AcquisitionMap
                             -> Frame
                             -> HypothesisMap [TrackID]
      intersectAcquisitions' []     _         _       _  _       _ = IDMap.empty
      intersectAcquisitions' (t:ts) trackMap' []      as acqMap' f = intersectAcquisitions' ts trackMap' as as acqMap' f
      intersectAcquisitions' (t:ts) trackMap' (a:as') as acqMap' f =
          if dist <= radius && dist /= 0 && delta <= time then
              -- track 'intersects' acquisition
              -- add the track ID to the map
              IDMap.insertWithKey (\a ts ts' -> ts ++ ts') a [t]
                       (intersectAcquisitions' (t:ts) trackMap' as' as acqMap' f)
          else
              intersectAcquisitions' (t:ts) trackMap' as' as acqMap' f
          where
            track                  = getItemFromMap trackMap' t
            (expectedX, expectedY) = trackExpectedLocation f track trackMap'
            acq                    = getItemFromMap acqMap' a
            acqX                   = acquisitionX acq
            acqY                   = acquisitionY acq
            dist                   = sqrt((expectedX - acqX)^2 + (expectedY - acqY)^2)
            radius                 = trackRadius (expectedX, expectedY) track trackMap'
            delta                  = trackDelta f track
            time                   = 0.7 -- seconds

-- | Human format of track log
showTracks :: [TrackID]      -- ^ Track IDs to log
           -> TrackMap       -- ^ Current track map
           -> AcquisitionMap -- ^ Current acquisition map
           -> Frame          -- ^ Current frame
           -> [String]       -- ^ Human log
showTracks []                  _        _       _ = []
showTracks (trackID:trackIDs) trackMap' acqMap' f =
    ["Track point " ++ (show trackID) ++
     " [acquisition: " ++ (show acqID) ++ ", " ++
     " x: " ++ (show $ acquisitionX acq) ++ ", " ++
     " y: " ++ (show $ acquisitionY acq) ++ ", " ++
     "\n\tprevious track point: " ++ (show trackID'') ++ ", " ++
     "\n\tnext track point: " ++ (show trackID') ++ ", " ++
     "\n\tduration: " ++ (printf "%.2f" $ trackDuration t trackMap') ++ " secs, " ++
     "\n\tspeed: " ++ (printf "%.2f" $ trackSpeed t trackMap') ++ ", " ++
     "\n\tdistance: " ++ (printf "%.2f" $ trackDistance t trackMap') ++ ", " ++
     "\n\texpected location: " ++ (show $ trackExpectedLocation f t trackMap') ++ ", " ++
     "\n\tscore: " ++ (show $ scoreTrack f trackID trackMap' Medium) ++ "]"]
    ++ showTracks trackIDs trackMap' acqMap' f
    where
      t                          = getItemFromMap trackMap' trackID
      (acq, trackID', trackID'') = -- track's acquisition, next track ID, prev track ID
          case t of
            Track acq _ (Just trackID') (Just trackID'') -> (acq, trackID', trackID'')
            Track acq _ (Just trackID') Nothing          -> (acq, trackID', 0)
            Track acq _ Nothing         (Just trackID'') -> (acq, 0, trackID'')
            Track acq _ Nothing         Nothing          -> (acq, 0, 0)
      acqID = foldWithKey (\k e a -> if e == acq then k else a) 0 acqMap'

instance Show Track where
    show (Track acq trackID n p) = "Track " ++ (show trackID)

-- | XML format of track log
tracksToXml :: [TrackID]       -- ^ Track IDs to log
            -> TrackMap        -- ^ Current track map
            -> Frame           -- ^ Current frame
            -> [HaXml.Content] -- ^ XML log
tracksToXml []                 _         _ = []
tracksToXml (trackID:trackIDs) trackMap' f =
    trackToXml t ++ tracksToXml trackIDs trackMap' f
    where
      t = getItemFromMap trackMap' trackID
      trackToXml (Track acq _ n p) =
          [worldElem "Track" [("id", show trackID),
                              ("x", show $ acquisitionX acq),
                              ("y", show $ acquisitionY acq),
                              ("ox", show $ acquisitionX acq'),
                              ("oy", show $ acquisitionY acq'),
                              ("prevID", show prevTrackID),
                              ("nextID", show nextTrackID),
                              ("ex", show expectedX),
                              ("ey", show expectedY),
                              ("radius", show radius),
                              ("thisFrame", show thisFrame)] []]
          where
            prevTrackID            = case p of
                                       Nothing          -> 0
                                       Just prevTrackID -> prevTrackID
            Track acq' _ _ _       = case p of
                                       Nothing          -> t
                                       Just prevTrackID -> getItemFromMap trackMap' prevTrackID
            nextTrackID            = case n of
                                       Nothing          -> 0
                                       Just nextTrackID -> nextTrackID
            (Frame attrs _)        = f
            thisFrame              = (frameTime attrs) == (acquisitionTime acq)
            (expectedX, expectedY) = if (acquisitionTime acq) /= (frameTime attrs) then
                                         (acquisitionX acq, acquisitionY acq)
                                     else
                                         trackExpectedLocation f t trackMap'
            radius                 = trackRadius (expectedX, expectedY) t trackMap'

-- | Keep only track hypotheses considered \'irrefutable\' or \'accepted\'
updateTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
updateTracks ws =
    recordWorldEvent (["Removed tracks:"] ++ (showTracks ((trackIDs ws) \\ newTrackIDs) (trackMap ws) (acqMap ws) (frame ws)) ++ ["END"], emptyElem) >>
                     return (ws { mind = newMind, hypIDs = newHypIDs, trackIDs = newTrackIDs, trackMap = newTrackMap })
    where
      m              = mind ws
      frametime      = let (Frame attrs _) = (frame ws) in frameTime attrs
      badHs          = IDSet.toList $ refutedHypotheses m
      goodHs         = IDSet.toList $ IDSet.union (irrefutableHypotheses m)
                       (IDSet.union (acceptedHypotheses m)
                             (IDSet.union (consideringHypotheses m) (unacceptableHypotheses m)))

      -- filter out refuted track heads and tracks whose head is older than 1 sec
      freshTrackMap  = foldl (\trackMap' h -> removeTrack (getItemFromMap trackMap' h) trackMap')
                       (trackMap ws)
                       (IDMap.keys (IDMap.filterWithKey (\h _ -> (elem h badHs) && (isTrackHead h (trackMap ws))) (trackMap ws)))
      freshTrackMap' = removeOldTracks (frame ws) (trackHeads freshTrackMap) freshTrackMap

      newTrackMap    = updateLinks freshTrackMap'
      newTrackIDs    = filter (\n -> elem n $ IDMap.keys newTrackMap) (trackIDs ws)
      newMind        = foldl (\m h -> removeHypothesis h m) (mind ws) ((trackIDs ws) \\ newTrackIDs)
      newHypIDs      = (IDMap.keys newTrackMap) ++ (IDMap.keys (noiseMap ws)) ++ (IDMap.keys (acqMap ws))

      removeOldTracks :: Frame -> [TrackID] -> TrackMap -> TrackMap
      removeOldTracks _ []     trackMap' = trackMap'
      removeOldTracks f (t:ts) trackMap' = if (trackDelta f (getItemFromMap trackMap' t)) <= 1.0 then -- track is 'fresh'
                                               removeOldTracks f ts trackMap'
                                           else -- remove entire track
                                               removeOldTracks f ts $ removeTrack (getItemFromMap trackMap' t) trackMap'

      -- remove a whole track (following 'previous' links)
      removeTrack :: Track -> TrackMap -> TrackMap
      removeTrack (Track _ trackID _ Nothing)         trackMap' = IDMap.delete trackID trackMap'
      removeTrack (Track _ trackID _ (Just trackID')) trackMap' = IDMap.delete trackID $
                                                                  if IDMap.member trackID' trackMap' then
                                                                      removeTrack (getItemFromMap trackMap' trackID') trackMap'
                                                                  else trackMap'

      -- update "next" and "prev" links in all tracks (since some track points may have been removed)
      updateLinks :: TrackMap -> TrackMap
      updateLinks trackMap' = foldl (\trackMap' trackID -> updateLink trackID 0 trackMap') trackMap' (trackHeads' trackMap')

      updateLink :: TrackID  -- ^ The new "next" link
                 -> TrackID  -- ^ Track to change
                 -> TrackMap
                 -> TrackMap
      -- initial call: second argument is 0, so get "prev" track from this head
      updateLink next 0 trackMap' = updateLink next trackID trackMap'
          where Track _ _ _ (Just trackID) = getItemFromMap trackMap' next
      updateLink next trackID trackMap' =
          if IDMap.member trackID trackMap' then
              let Track acq _ _ p = getItemFromMap trackMap' trackID in
              insert trackID (Track acq trackID (Just next) p) $
                     case p of
                       Nothing   -> trackMap'
                       Just prev -> updateLink trackID prev trackMap'
          else -- rewrite "next" track as having no previous track
              let Track acq _ n _ = getItemFromMap trackMap' next in insert next (Track acq next n Nothing) trackMap'

-- | Record recent track hypotheses
recordTracks :: WorldState       -- ^ World state
             -> World WorldState -- ^ Resulting world
recordTracks ws =
    recordWorldEvent (showTracks ts trackMap' acqMap' f,
                      recordWorldEventInFrame framenum frametime $ tracksToXml ts trackMap' f) >>
    return ws
    where
      ts                = keys (trackMap ws)
      trackMap'         = trackMap ws
      acqMap'           = acqMap ws
      f@(Frame attrs _) = frame ws
      framenum          = show $ frameNumber attrs
      frametime         = show $ frameTime attrs

-- | Construct a list of track heads
trackHeads :: TrackMap -> [TrackID]
trackHeads trackMap' = IDMap.keys $ IDMap.filter (\(Track _ _ n _) -> case n of
                                                                        Nothing -> True
                                                                        Just _  -> False) trackMap'

-- | Construct a list of head tracks that are not new tracks
trackHeads' :: TrackMap -> [TrackID]
trackHeads' trackMap' = IDMap.keys $ IDMap.filter (\(Track _ _ n p) -> case (n,p) of
                                                                         (Nothing, Just _) -> True
                                                                         otherwise         -> False) trackMap'

isTrackHead :: TrackID
            -> TrackMap
            -> Bool
isTrackHead trackID trackMap' = answer
    where
      (Track _ _ n _) = getItemFromMap trackMap' trackID
      answer          = n == Nothing

-- | Find the head of a track
trackHead :: Track
          -> TrackMap
          -> Track
trackHead t@(Track _ _ Nothing  _) _         = t
trackHead   (Track _ _ (Just n) _) trackMap' = trackHead (getItemFromMap trackMap' n) trackMap'

-- | Overall duration of track since origin
trackDuration :: Track  -- ^ Track of interest
              -> TrackMap
              -> Double -- ^ Track duration
trackDuration (Track acq _ _ Nothing)         _         = 0.0 -- no previous track
trackDuration (Track acq _ _ (Just trackID')) trackMap' = (acquisitionTime acq) - (acquisitionTime acq') + (trackDuration track' trackMap')
    where
      track'@(Track acq' _ _ _) = getItemFromMap trackMap' trackID'

-- | Track speed
--
-- Speed of track with most recent acquisition acq and second most recent acquisition acq\' =
-- (acqDistance acq acq\') (acqDelta acq acq\')
--
-- The maximum possible speed is 100 (forced limitation)
trackSpeed :: Track
           -> TrackMap
           -> Double
trackSpeed (Track acq _ _ Nothing)         _         = 0.0
trackSpeed (Track acq _ _ (Just trackID')) trackMap' = min 100 $ (acqDistance acq acq') / (acqDelta acq acq')
    where
      (Track acq' _ _ _) = getItemFromMap trackMap' trackID'

-- | Overall track distance since origin
trackDistance :: Track
              -> TrackMap
              -> Double
trackDistance (Track acq _ _ Nothing)         trackMap' = 0.0
trackDistance (Track acq _ _ (Just trackID')) trackMap' = (acqDistance acq acq') + (trackDistance track' trackMap')
    where
      track'@(Track acq' _ _ _) = getItemFromMap trackMap' trackID'

trackDelta :: Frame
           -> Track
           -> Double
trackDelta (Frame attrs _) (Track acq _ _ _) = (frameTime attrs) - (acquisitionTime acq)

trackExpectedLocation :: Frame
                      -> Track
                      -> TrackMap
                      -> (Double, Double)
trackExpectedLocation f   (Track acq _ _ Nothing)         _         = (acquisitionX acq, acquisitionY acq)
trackExpectedLocation f t@(Track acq _ _ (Just trackID')) trackMap' = (x3, y3)
    where
      (Track acq' _ _ _) = getItemFromMap trackMap' trackID'
      (x1, y1)           = (acquisitionX acq', acquisitionY acq') -- old position
      (x2, y2)           = (acquisitionX acq,  acquisitionY acq)  -- current position
      speed              = trackSpeed t trackMap'
      delta              = (trackDelta f t) + 0.333 -- add one frame's time
      dist               = speed * delta -- expected distance
      m                  = (y1 - y2) / (x2 - x1) -- y1 - y2 due to coordinate system
      (x3, y3) 
          | (y1 - y2) == 0 = if (x2 - x1) > 0 then -- moved right
                                 (x2 + dist, y2)
                             else                  -- moved left
                                 (x2 - dist, y2)
          | (x2 - x1) == 0 = if (y1 - y2) > 0 then -- moved up
                                 (x2, y2 - dist)
                             else                  -- moved down
                                 (x2, y2 + dist)
          | otherwise      = let x3 = if (x2 - x1) > 0 then -- moved right
                                          dist / ((sqrt 2) * m) + x2
                                      else                  -- moved left
                                          (0 - dist / ((sqrt 2) * m)) + x2
                             in (x3, y2 - m * (x3 - x2))

trackRadius :: (Double, Double) -> Track -> TrackMap -> Double
trackRadius (ex, ey) track@(Track acq _ _ _) trackMap'
    | speed == 0.0 = 75.0
    | speed < 50.0 = min (3 * expDist) $ 3 * speed
    | otherwise    = min (3 * expDist) $ 150.0
    where
      speed    = trackSpeed track trackMap'
      (x,y)    = (acquisitionX acq, acquisitionY acq)
      expDist  = sqrt ((x - ex)^2 + (y - ey)^2)

trackAcquisition :: TrackID -> TrackMap -> Acquisition
trackAcquisition trackID trackMap' = acq
    where
      Track acq _ _ _ = getItemFromMap trackMap' trackID