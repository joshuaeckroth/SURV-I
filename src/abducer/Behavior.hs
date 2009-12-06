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
                                     concatMap (\region -> map (\(agent, score) -> (path, region, agent, score))
                                                           agents)
                                     regions) $
                          associatePathsAndContext context entityMap paths
    in map (mkBehavior entityMap) pathsAndContext

mkBehavior :: HypothesisMap Entity -> (Path, String, String, Level) -> Hypothesis Behavior
mkBehavior entityMap (path, region, agent, s) =
    let content   = agent ++ " in " ++ region
        hypId     = mkBehaviorHypId content [path]
        pathRef   = extractPathHypId path
        score     = \_ -> s
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
                         -> [(Path, [String], [(String, Level)])] -- ^ (path, [region], [(agent, score)])
associatePathsAndContext _ _ [] = []
associatePathsAndContext context@(Context _ _ (Regions regions) _ (Agents agents)) entityMap (path:paths) =
    let assocRegions = findRegionIntersections entityMap regions path
        assocAgents  = findPossibleAgents entityMap agents path
    in [(path, assocRegions, assocAgents)] ++ associatePathsAndContext context entityMap paths

findRegionIntersections :: HypothesisMap Entity -> [Region] -> Path -> [String]
findRegionIntersections _ [] _ = []
findRegionIntersections entityMap (region@(Region (Region_Attrs regionName) _):regions) path =
    if pathIntersectsRegion entityMap path region
    then [regionName] ++ (findRegionIntersections entityMap regions path)
    else findRegionIntersections entityMap regions path

findPossibleAgents :: HypothesisMap Entity -> [Agent] -> Path -> [(String, Level)]
findPossibleAgents _ [] _ = []
findPossibleAgents entityMap (agent@(Agent agentName _ _):agents) path =
    let score = pathMatchesAgent entityMap path agent
    in [(agentName, score)] ++ (findPossibleAgents entityMap agents path)
