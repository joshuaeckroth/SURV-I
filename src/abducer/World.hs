{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module World where
import Types
import qualified Reasoner.Core as R
import Reasoner.Types (HypothesisID(..), HypothesisMap(..), HypothesisIDs, ExplainsID(..))
import qualified Reasoner.Constrainers as RC
import Vocabulary
import WrappedInts.Types (HasInt(..))
import qualified WrappedInts.IDSet as IDSet
import qualified WrappedInts.IDMap as IDMap
import Text.XML.HaXml.XmlContent (showXml)
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Dynamic
import Data.List (sortBy, intersect, (\\), nub, intercalate)
import Debug.Trace

data World = World { mind      :: R.Mind Level Level Level
                   , entityMap :: HypothesisMap Entity
                   }

mkWorld :: World
mkWorld = World { mind      = (R.setTrace True $ R.newMind confidenceBoost suggestStatus R.NoTransitive)
                , entityMap = IDMap.empty
                }

allHypotheses :: World -> HypothesisIDs
allHypotheses world = R.acceptedHypotheses (mind world)
                      `IDSet.union` (R.consideringHypotheses (mind world))
                      `IDSet.union` (R.refutedHypotheses (mind world))

rejectedHypotheses :: World -> HypothesisIDs
rejectedHypotheses world = R.refutedHypotheses (mind world)

cleanWorld :: World -> World
cleanWorld world = 
    let allDetHypIds       = map extractDetHypId $ gatherEntities (entityMap world) (allHypotheses world)
        newerDetHypIds     = newerDetections (entityMap world) (allHypotheses world)
        oldDetHypIds       = allDetHypIds \\ newerDetHypIds
        invalidMovHypIds   = invalidMovements oldDetHypIds (entityMap world) (allHypotheses world)
        allRejPathHypIds   = map extractPathHypId $ gatherEntities (entityMap world) (rejectedHypotheses world)
        newerRejPathHypIds = newerPaths (entityMap world) (rejectedHypotheses world)
        oldRejPathHypIds   = allRejPathHypIds \\ newerRejPathHypIds
        removable          = oldDetHypIds ++ invalidMovHypIds ++ oldRejPathHypIds
    in foldr removeHypothesis world removable

newerDetections :: HypothesisMap Entity
                -> HypothesisIDs
                -> [HypothesisID]
newerDetections entityMap hypIds =
    take 40 $ map extractDetHypId $
    reverse $ sortBy detAscOrdering $
    gatherEntities entityMap hypIds

newerPaths :: HypothesisMap Entity
           -> HypothesisIDs
           -> [HypothesisID]
newerPaths entityMap hypIds =
    let -- convert paths into (path, detStart) pairs
        pathAndDetStarts = map (\(path, movs) ->
                                    (path, let (Movement _ detStartHypId _ _) = head movs in
                                           getEntity entityMap detStartHypId)) $
                           map (\path@(Path _ (NonEmpty movRefs)) ->
                                (path, map (\movRef -> getEntity entityMap $ movementRefMovId movRef) movRefs)) $
                           gatherEntities entityMap hypIds
    in take 20 $ map (extractPathHypId . fst) $
       reverse $ sortBy (\(path1, detStart1) (path2, detStart2) -> detAscOrdering detStart1 detStart2)
       pathAndDetStarts
                                       


invalidMovements :: [HypothesisID]
                 -> HypothesisMap Entity
                 -> HypothesisIDs
                 -> [HypothesisID]
invalidMovements detHypIds entityMap hypIds =
    map extractMovHypId $
    filter (\(Movement _ detId1 detId2 _) -> (elem detId1 detHypIds) || (elem detId2 detHypIds))
    (gatherEntities entityMap hypIds :: [Movement])

invalidPaths :: [HypothesisID]
             -> HypothesisMap Entity
             -> HypothesisIDs
             -> [HypothesisID]
invalidPaths movHypIds entityMap hypIds =
    map extractPathHypId $
    filter (\(Path _ (NonEmpty movRefs)) ->
        (not $ null $ intersect (map (\(MovementRef movId) -> movId) movRefs) movHypIds))
    (gatherEntities entityMap hypIds :: [Path])

-- | Remove paths that are entirely contained in other paths.
-- 
-- Such \"subpaths\" are completely irrelevant to decision making; that is,
-- since paths must continually grow to stay relevant, there is not benefit
-- in keeping subpaths. Keeping subpaths causes a very significant growth in
-- the number of path hypotheses the reasoner is forced to reconcile.
removeSubPaths :: World -> World
removeSubPaths world = foldr removeHypothesis world (map extractPathHypId subPaths)
    where paths    = gatherEntities (entityMap world) (allHypotheses world)
          subPaths = nub $ foldl (++) [] (map (findSubPaths paths) paths)

findSubPaths :: [Path] -> Path -> [Path]
findSubPaths paths path = filter (isSubPath path) paths

isSubPath :: Path -> Path -> Bool
isSubPath (Path (Path_Attrs hypId1 _ _) (NonEmpty movRefs1))
              (Path (Path_Attrs hypId2 _ _) (NonEmpty movRefs2))
    -- a path is not a subpath of itself
    | hypId1 == hypId2   = False
    -- a path is a subpath of another if all of the movements of the first
    -- path are shared with the second
    | movsNotShared == 0 = True
    | otherwise          = False
    where movsNotShared = length $ movRefs1 \\ movRefs2

-- | Look at all paths and determine which subsets conflict (are \"rivals\"); add
--   these conflicts into the path hypotheses.
-- 
-- Two paths conflict or are rivals if they share a significant (> 80%) number
-- of movements.
updateConflictingPaths :: World -> World
updateConflictingPaths world = mergeIntoWorld world $ findConflictingPaths paths paths
    where
      paths = gatherEntities (entityMap world) (allHypotheses world)

      -- remove all conflict relations for all paths,
      -- then add conflict relations for each (path, conflicts) pair
      mergeIntoWorld :: World -> [(HypothesisID, [HypothesisID])] -> World
      mergeIntoWorld world conflicts =
          let -- first add the conflicts relations
              world1 = foldl (\w (subject, objects) ->
                              foldl (\w' object ->
                                     addConflicts subject object w') w objects)
                       (removePathConflicts world (map fst conflicts)) conflicts
              -- then update the path entities to record their conflicts
              world2 = foldl (\w (subject, objects) ->
                              let (Path (Path_Attrs hypId score _) movs) = getEntity (entityMap w) subject in
                              w {entityMap = IDMap.insert subject
                                             (toDyn $ (Path (Path_Attrs hypId score (intercalate "," (map show objects))) movs))
                                             (entityMap w)}) world1 conflicts
          in world2

      -- remove all adjusting (conflict) relations exclusively involving paths
      -- FIXME: this is very inefficient!
      removePathConflicts :: World -> [HypothesisID] -> World
      removePathConflicts world hypIds =
          foldl (\w (subject, object) ->
                 w {mind = R.removeAdjuster (mkConflictsId subject object) (mind w)})
                    world [(subject, object) | subject <- hypIds, object <- hypIds]

findConflictingPaths :: [Path] -> [Path] -> [(HypothesisID, [HypothesisID])]
findConflictingPaths [] _ = []
findConflictingPaths (path@(Path (Path_Attrs hypId _ _) _):paths) paths' =
    [(hypId, map extractPathHypId $ filter (pathsConflict path) paths')]
    ++ (findConflictingPaths paths paths')

pathsConflict :: Path -> Path -> Bool
pathsConflict (Path (Path_Attrs hypId1 _ _) (NonEmpty movRefs1))
                  (Path (Path_Attrs hypId2 _ _) (NonEmpty movRefs2))
    -- a path does not conflict with itself
    | hypId1 == hypId2       = False
    -- a path conflicts with another path if the one shares at least 50% of
    -- the same movements with the other
    | movsNotShared12 <= 0.5 = True
    | movsNotShared21 <= 0.5 = True
    -- otherwise there is no conflict
    | otherwise              = False
    where movsNotShared12 = (fromIntegral $ length $ movRefs1 \\ movRefs2) /
                            (fromIntegral $ length movRefs1)
          movsNotShared21 = (fromIntegral $ length $ movRefs2 \\ movRefs1) /
                            (fromIntegral $ length movRefs2)

hypothesize :: (Typeable a) => [Hypothesis a] -> World -> World
hypothesize hs world =
    let world1 = foldl (\w h -> addHypothesis (entity h) (hypId h) (aPriori h) w) world hs
        world2 = foldl (\w (Hyp _ subject _ explains _ _) ->
                        foldl (\w' object ->
                               addExplains subject object w') w explains) world1 hs
        world3 = foldl (\w (Hyp _ subject _ _ implies _) ->
                        foldl (\w' object ->
                               addImplies subject object w') w implies) world2 hs
        world4 = foldl (\w (Hyp _ subject _ _ _ conflicts) ->
                        foldl (\w' object ->
                               addConflicts subject object w') w conflicts) world3 hs
    in world4

addHypothesis :: (Typeable a) => a -> HypothesisID -> (Level -> Level) -> World -> World
addHypothesis entity hypId scoreFunc world =
    let entityMap' = IDMap.insert hypId (toDyn entity) (entityMap world)
    in
    world { mind      = R.addHypothesis hypId category scoreFunc (mind world)
          , entityMap = entityMap'
          }

-- | Remove a hypothesis from the \"mind\" but not from the entity map.
-- 
-- We keep the entity in the map because paths, for example, need to be
-- able to refer to movements that are too old to \"reason\" about but
-- are still members of valid paths.
removeHypothesis :: HypothesisID -> World -> World
removeHypothesis hypId world = world { mind = R.removeHypothesis hypId (mind world) }

-- | Add an explains relationship between two hypotheses.
-- 
-- Do not add this relationship if the object of the explains relationship
-- is no longer a hypothesis; this can occur when a path is extended but
-- some of its earlier movements have been removed from consideration;
-- in this case, the path will attempt to add an explaining relationship
-- to all of its movements, but some movements will not exist (as hypotheses).
addExplains :: HypothesisID -> HypothesisID -> World -> World
addExplains subject object world =
    if IDSet.member object (allHypotheses world) then
        world { mind = R.addExplains (mkExplainsId subject object)
                       subject object (mind world)
              }
    else world

-- | Add an implies relationship between two hypotheses.
-- 
-- We check if the object is a valid hypothesis; look at the documentation
-- for 'addExlains' for more information about this requirement.
addImplies :: HypothesisID -> HypothesisID -> World -> World
addImplies subject object world =
    if IDSet.member object (allHypotheses world) then
        world { mind = R.addAdjuster (mkImpliesId subject object)
                       subject boostOnAcceptance object (mind world)
              }
    else world

-- | Add a conflicts relationship between two hypotheses.
-- 
-- We check if the object is a valid hypothesis; look at the documentation
-- for 'addExplains' for more information about this requirement.
addConflicts :: HypothesisID -> HypothesisID -> World -> World
addConflicts subject object world =
    if IDSet.member object (allHypotheses world) then
        world { mind = R.addAdjuster (mkConflictsId subject object)
                       subject lowerOnAcceptance object (mind world)
              }
    else world

boostOnAcceptance = Left (Just $ increaseLevelBy 2, Nothing)
lowerOnAcceptance = Left (Just $ decreaseLevelBy 2, Nothing)

reason :: World -> World
reason world = world { mind = R.reason (R.ReasonerSettings False) Medium (mind world) }

addToEntityMap :: forall a.
                  (Typeable a) =>
                  HypothesisMap Entity
               -> (HypothesisID, a)
               -> HypothesisMap Entity
addToEntityMap emap (hypId, entity) = if IDMap.member hypId emap then emap
                                      else IDMap.insert hypId (toDyn entity) emap

getEntity :: forall a.
             (Typeable a) =>
             HypothesisMap Entity
          -> HypothesisID
          -> a
getEntity entityMap hypId = let (Just entity) = fromDynamic $ (IDMap.!) entityMap hypId
                            in entity

gatherEntities :: forall a.
                  (Typeable a) =>
                  HypothesisMap Entity
               -> HypothesisIDs
               -> [a]
gatherEntities entityMap hypIds =
    map (\(Just entity) -> entity) $
    map (\dyn -> fromDynamic dyn) $
    filter (\dyn -> case (fromDynamic dyn :: Maybe a) of
                      Just entity -> True
                      Nothing     -> False) $
    IDMap.elems $
    IDMap.filterWithKey (\hypId _ -> IDSet.member hypId hypIds) entityMap

buildResults :: World -> Results
buildResults world =
    let accepted = R.acceptedHypotheses (mind world)
        rejected = R.refutedHypotheses (mind world)
        -- include movements and detections referred to by current paths but that have been
        -- removed from the known hypotheses (because they are old)
        pathMovs = gatherEntities (entityMap world) $ IDSet.fromList $
                   nub $ concat $ map (\(Path _ (NonEmpty movRefs)) -> map movementRefMovId movRefs)
                   (gatherEntities (entityMap world) (allHypotheses world))
        pathDets = gatherEntities (entityMap world) $ IDSet.fromList $
                   nub $ concat $ map (\(Movement _ det1 det2 _) -> [det1, det2]) pathMovs
    in
      Results (Entities (nub $ pathDets ++ (gatherEntities (entityMap world) (allHypotheses world)))
                        (nub $ pathMovs ++ (gatherEntities (entityMap world) (allHypotheses world)))
                        (gatherEntities (entityMap world) (allHypotheses world)))
                  (Accepted (map (mkDetectionRef . extractDetHypId) $ gatherEntities (entityMap world) accepted)
                            (map (mkMovementRef . extractMovHypId) $ gatherEntities (entityMap world) accepted)
                            (map (mkPathRef . extractPathHypId) $ gatherEntities (entityMap world) accepted))
                  (Rejected (map (mkDetectionRef . extractDetHypId) $ gatherEntities (entityMap world) rejected)
                            (map (mkMovementRef . extractMovHypId) $ gatherEntities (entityMap world) rejected)
                            (map (mkPathRef . extractPathHypId) $ gatherEntities (entityMap world) rejected))

outputLog :: World -> String
outputLog world = showXml False $ buildResults world

