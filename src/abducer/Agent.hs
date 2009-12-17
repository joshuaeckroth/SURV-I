module Agent
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

mkAgents :: Context -> HypothesisMap Entity -> [Path] -> [Hypothesis Agent]
mkAgents context entityMap paths =
    let pathsAndAgents = concatMap (\(path, agents) ->
                                    map (\(agent, score) -> (path, agent, score)) agents) $
                         associatePathsAndAgents context entityMap paths
    in updateConflicts $ map (mkAgent entityMap) (filter (\(_, _, score) -> score /= Low) pathsAndAgents)

mkAgent :: HypothesisMap Entity -> (Path, String, Level) -> Hypothesis Agent
mkAgent entityMap (path, a, s) =
    let content   = a
        hypId     = mkAgentHypId content [path]
        pathRef   = extractPathHypId path
        score     = \_ -> s
        agent     = Agent (Agent_Attrs hypId (show $ score Medium) content "")
                    (NonEmpty $ map mkPathRef [pathRef])
        aPriori   = score
        explains  = [pathRef]
        implies   = [pathRef]
        conflicts = []
    in Hyp agent hypId aPriori explains implies conflicts

updateConflicts :: [Hypothesis Agent] -> [Hypothesis Agent]
updateConflicts hAgents = map (updateConflicts' hAgents) hAgents
    where
      updateConflicts' :: [Hypothesis Agent] -> Hypothesis Agent -> Hypothesis Agent
      updateConflicts' hAgents hAgent@(Hyp (Agent attrs pathRefs) hypId _ (pathRef:[]) _ _) =
          let cs = map (\(Hyp {hypId = hypId}) -> hypId) $
                   filter (\(Hyp _ hypId' _ (pathRef':[]) _ _) -> hypId /= hypId' && pathRef == pathRef') hAgents
          in hAgent { entity = (Agent (attrs { agentConflicts = intercalate "," (map show cs) }) pathRefs), conflicts = cs }

associatePathsAndAgents :: Context
                        -> HypothesisMap Entity
                        -> [Path]
                        -> [(Path, [(String, Level)])]
associatePathsAndAgents _ _ [] = []
associatePathsAndAgents context@(Context _ _ _ _ (AgentTemplates agentTemplates)) entityMap (path:paths) =
    let assocAgents = findPossibleAgents entityMap agentTemplates path
    in [(path, assocAgents)] ++ associatePathsAndAgents context entityMap paths

findPossibleAgents :: HypothesisMap Entity -> [AgentTemplate] -> Path -> [(String, Level)]
findPossibleAgents _ [] _ = []
findPossibleAgents entityMap (agentTemplate@(AgentTemplate agentName _ _):agentTemplates) path =
    let score = pathMatchesAgent entityMap path agentTemplate
    in [(agentName, score)] ++ (findPossibleAgents entityMap agentTemplates path)

pathMatchesAgent :: HypothesisMap Entity -> Path -> AgentTemplate -> Level
pathMatchesAgent entityMap path (AgentTemplate _ area speed)
    | ratio >= 0.5  && ratio <= 2.0 = SlightlyHigh
    | ratio >= 0.25 && ratio <= 4.0 = Medium
    | otherwise                     = Low
    where areaRatio  = area / pathAverageArea entityMap path
          speedRatio = speed / pathAverageSpeed entityMap path
          ratio      = (areaRatio + speedRatio) / 2.0

