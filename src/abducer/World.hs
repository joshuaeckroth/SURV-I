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
import Data.List (sortBy, (\\))
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

cleanWorld :: World -> World
cleanWorld world = 
    let allDetHypIds      = map detectionId $ gatherEntities (entityMap world) (allHypotheses world)
        newerDetHypIds    = newerDetections (entityMap world) (allHypotheses world)
        oldDetHypIds      = allDetHypIds \\ newerDetHypIds
        invalidMovHypIds  = invalidMovements oldDetHypIds (entityMap world) (allHypotheses world)
        invalidPathHypIds = invalidPaths invalidMovHypIds (entityMap world) (allHypotheses world)
        removable         = oldDetHypIds ++ invalidMovHypIds ++ invalidPathHypIds
    in foldr removeHypothesis world removable

newerDetections :: HypothesisMap Entity
                -> HypothesisIDs
                -> [HypothesisID]
newerDetections entityMap allHypotheses =
    take 20 $ map detectionId $
    reverse $ sortBy detAscOrdering $
    (gatherEntities entityMap allHypotheses :: [Detection])

invalidMovements :: [HypothesisID]
                 -> HypothesisMap Entity
                 -> HypothesisIDs
                 -> [HypothesisID]
invalidMovements detHypIds entityMap allHypotheses =
    map (\(Movement (Movement_Attrs hypId) _) -> hypId) $
    filter (\(Movement _ (NonEmpty (det1:det2:[]))) ->
                (elem (detectionId det1) detHypIds) || (elem (detectionId det2) detHypIds))
    (gatherEntities entityMap allHypotheses :: [Movement])

invalidPaths :: [HypothesisID]
             -> HypothesisMap Entity
             -> HypothesisIDs
             -> [HypothesisID]
invalidPaths movHypIds entityMap allHypotheses =
    map (\(Path (Path_Attrs hypId) _) -> hypId) $
    filter (\(Path _ (NonEmpty movs)) ->
        (null $ (\\) (map (\(Movement (Movement_Attrs movId) _) -> movId) movs) movHypIds))
    (gatherEntities entityMap allHypotheses :: [Path])

hypothesize :: (Typeable a) => [Hypothesis a] -> World -> World
hypothesize hs world =
    let world1 = foldl (\w h -> addHypothesis (entity h) (hypId h) (aPriori h) w) world hs
        world2 = foldl (\w (Hyp _ subject _ explains _ _) ->
                            foldl (\w' object ->
                                   addExplains subject object w') w explains) world1 hs
        world3 = foldl (\w (Hyp _ subject _ _ implies _) ->
                            foldl (\w' object ->
                                   addImplies subject object w') w implies) world2 hs
    in world3

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

addRefutes :: HypothesisID -> HypothesisID -> World -> World
addRefutes subject object world =
    world { mind = R.addAdjuster (mkHypPairId object subject)
                   object refuteLower subject (mind world)
          }

boostOnAcceptance = Left (Just $ increaseLevelBy 2, Nothing)

refuteLower = Left (Nothing, Just $ decreaseLevelBy 2)

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

