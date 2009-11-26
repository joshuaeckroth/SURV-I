module Behavior
where

import Types
import Context
import Vocabulary
import Path
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List (intercalate)
import Debug.Trace

mkBehaviors :: Context -> HypothesisMap Entity -> [Path] -> [Hypothesis Behavior]
mkBehaviors context entityMap paths =
    let pathsAndContext = concatMap (\(path, regions, agents) ->
                                     concatMap (\region -> map (\agent -> (path, region, agent))
                                                           agents)
                                     regions) $
                          associatePathsAndContext context entityMap paths
    in map (mkBehavior entityMap) pathsAndContext

mkBehavior :: HypothesisMap Entity -> (Path, String, String) -> Hypothesis Behavior
mkBehavior entityMap (path, region, agent) =
    let content   = agent ++ " in " ++ region
        hypId     = mkBehaviorHypId content [path]
        pathRef   = extractPathHypId path
        score     = \_ -> High
        behavior  = Behavior (Behavior_Attrs hypId (show $ score Medium) content)
                    (NonEmpty $ map mkPathRef [pathRef])
        aPriori   = score
        explains  = [pathRef]
        implies   = [pathRef]
        conflicts = []
    in Hyp behavior hypId aPriori explains implies conflicts

associatePathsAndContext :: Context
                         -> HypothesisMap Entity
                         -> [Path]
                         -> [(Path, [String], [String])] -- ^ (path, [region], [agent])
associatePathsAndContext _ _ [] = []
associatePathsAndContext context@(Context _ _ (Regions regions) _ (Agents agents)) entityMap (path:paths) =
    [(path, findRegionIntersections entityMap regions path, findPossibleAgents entityMap agents path)] ++
    associatePathsAndContext context entityMap paths

findRegionIntersections :: HypothesisMap Entity -> [Region] -> Path -> [String]
findRegionIntersections _ [] _ = []
findRegionIntersections entityMap (region@(Region (Region_Attrs regionName) _):regions) path =
    if pathIntersectsRegion entityMap path region
    then [regionName] ++ (findRegionIntersections entityMap regions path)
    else findRegionIntersections entityMap regions path

findPossibleAgents :: HypothesisMap Entity -> [Agent] -> Path -> [String]
findPossibleAgents _ [] _ = []
findPossibleAgents entityMap (agent@(Agent agentName _ _ _ _):agents) path =
    if pathMatchesAgent entityMap path agent
    then [agentName] ++ (findPossibleAgents entityMap agents path)
    else findPossibleAgents entityMap agents path
