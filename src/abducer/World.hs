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
import Data.List (sortBy, intersect, (\\), nub)
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
    let allDetHypIds       = map detectionId $ gatherEntities (entityMap world) (allHypotheses world)
        newerDetHypIds     = newerDetections (entityMap world) (allHypotheses world)
        oldDetHypIds       = allDetHypIds \\ newerDetHypIds
        invalidMovHypIds   = invalidMovements oldDetHypIds (entityMap world) (allHypotheses world)
        invalidPathHypIds  = invalidPaths invalidMovHypIds (entityMap world) (allHypotheses world)
        removable          = oldDetHypIds ++ invalidMovHypIds ++ invalidPathHypIds
    in foldr removeHypothesis world removable

newerDetections :: HypothesisMap Entity
                -> HypothesisIDs
                -> [HypothesisID]
newerDetections entityMap hypIds =
    take 15 $ map detectionId $
    reverse $ sortBy detAscOrdering $
    (gatherEntities entityMap hypIds :: [Detection])

invalidMovements :: [HypothesisID]
                 -> HypothesisMap Entity
                 -> HypothesisIDs
                 -> [HypothesisID]
invalidMovements detHypIds entityMap hypIds =
    map (\(Movement (Movement_Attrs hypId) _) -> hypId) $
    filter (\(Movement _ (NonEmpty (det1:det2:[]))) ->
                (elem (detectionId det1) detHypIds) || (elem (detectionId det2) detHypIds))
    (gatherEntities entityMap hypIds :: [Movement])

invalidPaths :: [HypothesisID]
             -> HypothesisMap Entity
             -> HypothesisIDs
             -> [HypothesisID]
invalidPaths movHypIds entityMap hypIds =
    map (\(Path (Path_Attrs hypId) _) -> hypId) $
    filter (\(Path _ (NonEmpty movs)) ->
        (not $ null $ intersect (map (\(Movement (Movement_Attrs movId) _) -> movId) movs) movHypIds))
    (gatherEntities entityMap hypIds :: [Path])

-- | Remove paths that are entirely contained in other paths.
-- 
-- Such \"subpaths\" are completely irrelevant to decision making; that is,
-- since paths must continually grow to stay relevant, there is not benefit
-- in keeping subpaths. Keeping subpaths causes a very significant growth in
-- the number of path hypotheses the reasoner is forced to reconcile.
removeSubPaths :: World -> World
removeSubPaths world = foldr removeHypothesis world (map (\(Path (Path_Attrs hypId) _) -> hypId) subPaths)
    where paths    = gatherEntities (entityMap world) (allHypotheses world)
          subPaths = nub $ foldl (++) [] (map (findSubPaths paths) paths)

          findSubPaths :: [Path] -> Path -> [Path]
          findSubPaths paths path = filter (isSubPath path) paths

          isSubPath :: Path -> Path -> Bool
          isSubPath (Path (Path_Attrs hypId1) (NonEmpty movs1))
                        (Path (Path_Attrs hypId2) (NonEmpty movs2))
              -- a path is not a subpath of itself
              | hypId1 == hypId2      = False
              -- a path is a subpath if all of the latter path's movements are
              -- in the former path
              | null $ movs2 \\ movs1 = True
              | otherwise             = False

-- | Look at all paths and determine which subsets conflict (are \"rivals\"); add
--   these conflicts into the path hypotheses.
-- 
-- Two paths conflict or are rivals if they share a significant (> 80%) number
-- of movements.
updateConflictingPaths :: World -> World
updateConflictingPaths world = mergeIntoWorld world $ findConflictingPaths' paths paths
    where
      paths = gatherEntities (entityMap world) (allHypotheses world)

      -- remove all conflict relations for all paths,
      -- then add conflict relations for each (path, conflicts) pair
      mergeIntoWorld :: World -> [(HypothesisID, [HypothesisID])] -> World
      mergeIntoWorld world conflicts =
          foldl (\w (subject, objects) ->
                 foldl (\w' object ->
                        addConflicts subject object w') w objects)
          (removePathConflicts world (map fst conflicts)) conflicts

      -- remove all adjusting (conflict) relations exclusively involving paths
      -- FIXME: this is very inefficient!
      removePathConflicts :: World -> [HypothesisID] -> World
      removePathConflicts world hypIds =
          foldl (\w (subject, object) ->
                 w {mind = R.removeAdjuster (mkHypPairId subject object) (mind w)})
                    world [(subject, object) | subject <- hypIds, object <- hypIds]

      findConflictingPaths' :: [Path] -> [Path] -> [(HypothesisID, [HypothesisID])]
      findConflictingPaths' [] _ = []
      findConflictingPaths' (path@(Path (Path_Attrs hypId) _):paths) paths' =
          [(hypId, map (\(Path (Path_Attrs hypId') _) -> hypId') $
                 filter (pathsConflict path) paths')]
          ++ (findConflictingPaths' paths paths')

      pathsConflict :: Path -> Path -> Bool
      pathsConflict (Path (Path_Attrs hypId1) (NonEmpty movs1))
                        (Path (Path_Attrs hypId2) (NonEmpty movs2))
          -- a path does not conflict with itself
          | hypId1 == hypId2   = False
          -- a path conflicts with another path if at least one path shares > 80% of
          -- the same movements with the other
          | movsShared12 > 0.8 = True
          | movsShared21 > 0.8 = True
          -- otherwise there is no conflict
          | otherwise          = False
          where movsShared12 = (fromIntegral $ length $ movs1 \\ movs2) / (fromIntegral $ length movs1)
                movsShared21 = (fromIntegral $ length $ movs2 \\ movs1) / (fromIntegral $ length movs2)

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

removeHypothesis :: HypothesisID -> World -> World
removeHypothesis hypId world =
    world { mind      = R.removeHypothesis hypId (mind world)
          , entityMap = IDMap.delete hypId (entityMap world)
          }

addExplains :: HypothesisID -> HypothesisID -> World -> World
addExplains subject object world =
    world { mind = R.addExplains (mkHypPairId subject object)
                   subject object (mind world)
          }

addImplies :: HypothesisID -> HypothesisID -> World -> World
addImplies subject object world =
    world { mind = R.addAdjuster (mkHypPairId subject object)
                   subject boostOnAcceptance object (mind world)
          }

addConflicts :: HypothesisID -> HypothesisID -> World -> World
addConflicts subject object world =
    world { mind = R.addAdjuster (mkHypPairId subject object)
                   subject lowerOnAcceptance object (mind world)
          }

boostOnAcceptance = Left (Just $ increaseLevelBy 2, Nothing)
lowerOnAcceptance = Left (Just $ decreaseLevelBy 2, Nothing)

reason :: World -> World
reason world = world { mind = R.reason (R.ReasonerSettings True) Medium (mind world) }

buildResults :: World -> Results
buildResults world =
    let accepted = R.acceptedHypotheses (mind world)
        rejected = R.refutedHypotheses (mind world)
    in
      Results (Accepted (gatherEntities (entityMap world) accepted)
                            (gatherEntities (entityMap world) accepted)
                            (gatherEntities (entityMap world) accepted))
                  (Rejected (gatherEntities (entityMap world) rejected)
                                (gatherEntities (entityMap world) rejected)
                                (gatherEntities (entityMap world) rejected))

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

outputLog :: World -> String
outputLog world = showXml False $ buildResults world
--                  "<Log>" ++ (unlines $ R.showMind (mind world)) ++ "</Log>\n"

