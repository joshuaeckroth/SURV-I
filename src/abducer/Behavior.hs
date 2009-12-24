module Behavior
where

import Types
import Context
import Vocabulary
import Path
import Agent
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import Reasoner.Types (HypothesisMap(..))
import World (getEntity)
import Data.List (intercalate, sort)
import Debug.Trace

mkBehaviors :: Context -> HypothesisMap Entity -> [Agent] -> [Hypothesis Behavior]
mkBehaviors context entityMap agents =
    map (mkBehavior context entityMap) $ associateAgentsAndContext context entityMap agents

mkBehavior :: Context -> HypothesisMap Entity -> (Agent, [String]) -> Hypothesis Behavior
mkBehavior (Context _ _ _ (PointsOfInterest pois) _) entityMap (agent@(Agent agentAttrs _), regions) =
    let content         = (agentContent agentAttrs) ++ " in " ++ (intercalate "/" $ sort regions)
        nearPois        = findPointsOfInterest entityMap pois agent
        contentWithPois = content ++ (if null nearPois then "" else ", heading " ++ (intercalate ", " nearPois))
        hypId           = mkBehaviorHypId content [agent]
        agentRef        = extractAgentHypId agent
        score           = \_ -> High
        behavior        = Behavior (Behavior_Attrs hypId (show $ score Medium) contentWithPois "")
                          (NonEmpty $ map mkAgentRef [agentRef])
        aPriori         = score
        explains        = [agentRef]
        implies         = []
        conflicts       = []
    in Hyp behavior hypId aPriori explains implies conflicts

associateAgentsAndContext :: Context
                         -> HypothesisMap Entity
                         -> [Agent]
                         -> [(Agent, [String])] -- ^ (agent, [region])
associateAgentsAndContext _ _ [] = []
associateAgentsAndContext context@(Context _ _ (Regions regions) _ _) entityMap (agent:agents) =
    let assocRegions = findRegionIntersections entityMap regions agent
    in [(agent, assocRegions)] ++ associateAgentsAndContext context entityMap agents

findRegionIntersections :: HypothesisMap Entity -> [Region] -> Agent -> [String]
findRegionIntersections _ [] _ = []
findRegionIntersections entityMap (region@(Region (Region_Attrs regionName) _):regions) agent@(Agent _ (NonEmpty (pathRef:[]))) =
    let path = getEntity entityMap (pathRefPathId pathRef) in
    if pathIntersectsRegion entityMap path region
    then [regionName] ++ (findRegionIntersections entityMap regions agent)
    else findRegionIntersections entityMap regions agent

findPointsOfInterest :: HypothesisMap Entity -> [PointOfInterest] -> Agent -> [String]
findPointsOfInterest _ [] _ = []
findPointsOfInterest entityMap (poi@(PointOfInterest name _ _ _):pois) agent@(Agent _ (NonEmpty (pathRef:[]))) =
    let path                 = getEntity entityMap (pathRefPathId pathRef)
        (inRange, qualifier) = pathHeadNearPointOfInterest entityMap poi path in
    if inRange then [qualifier ++ " " ++ name] ++ (findPointsOfInterest entityMap pois agent)
    else findPointsOfInterest entityMap pois agent
