{-# LANGUAGE ScopedTypeVariables #-} -- only needed to add foralls to permit some optional type signatures

{-| The "Reasoner.Core" module is the core of where domain-independent
    abductive inference happens. It is Copyright 2007, 2008 by Aetion
    Technologies LLC and is proprietary and company confidential. -}

module Reasoner.Core
 ({-* Data types -}
  TransitiveExplains(..), Mind, HypEvent(..), MindEvent(..),
  {-* Querying a 'Mind' -}
  showMind, showHypothesis, hypothesisStatus, unexplained, mindTrace, hypothesisTrace, setTrace,
  unacceptableHypotheses, refutedHypotheses, consideringHypotheses, acceptedHypotheses, irrefutableHypotheses,
  getAdjusters, getConstrainers, getExplainers,
  {-* Creating and manipulating a 'Mind' -}
  newMind, addHypothesis, removeHypothesis,
  addAdjuster, removeAdjuster, addConstrainer, removeConstrainer, addExplains, removeExplains,
  setCategory, setConfidence, unsetConfidence, setFactual, setCounterfactual, unsetStatus, calculateDependencies,
  {-* Reasoning -}
  ReasonerSettings(..), reason)

where
import Control.Monad (liftM)
import Data.Foldable as Foldable (toList)
import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import WrappedInts.IDMap (getItemFromMap, getSetFromMap, removeFromMapSet)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import Reasoner.Types
import WrappedInts.Types

-- Data types

{-| An 'MindEvent' is used as a component of a sequenced log of the activity
    in a mind. -}

data MindEvent c =
    ME_HypothesisEvent HypothesisID (HypEvent c)
  | ME_HypothesesEvent HypothesisIDs (HypEvent c)
  | ME_AddHypothesis HypothesisID
  | ME_RemoveHypothesis HypothesisID
  | ME_AddAdjuster AdjusterID SubjectID ObjectID
  | ME_RemoveAdjuster AdjusterID
  | ME_AddExplains ExplainsID SubjectID ObjectID
  | ME_RemoveExplains ExplainsID
  | ME_StartedReasoning ReasonerSettings
  | ME_ReinvokingReasoner
  | ME_FinishedReasoning
    deriving (Eq, Show)

{-| A 'HypEvent' is used as a component of a sequenced log of the activity in
    a hypothesis; the log is kept in the 'Mind' in which the hypothesis is. A
    setting of a thing to Nothing is actually an unsetting. -}

data HypEvent c =
    HE_NeedsEvaluation
  | HE_StartedEvaluation
  | HE_CategorySet CategoryID
  | HE_APrioriConfidence c
  | HE_ConfidenceAdjustment AdjusterID c
  | HE_ConfidenceBoost ObjectID c
  | HE_ConfidenceConstraint ConstrainerIDs (c, c)
  | HE_ConfidenceOverride (Maybe c)
  | HE_BeliefStatusOverride (Maybe Status)
  | HE_BeliefStatusSet Status
  | HE_FinishedEvaluation
    deriving (Eq, Show)

{-| 'TransitiveExplains' specific how the 'Mind' should behave toward chains
    of explains links. -}

data TransitiveExplains =
    NoTransitive     {- ^ Do not follow explains relations transitively -}
  | SparseTransitive {- ^ Follow explains relations transitively without a cache -}
  | DenseTransitive  {- ^ Build a cache and use it to follow explains relations transitively -}
    deriving (Eq, Show)

{-| A 'Mind' holds the state of a reasoning process, including the hypotheses
    and how they are regarded, and the whole evidence network. -}

-- TODO: Write sanity checker to check for all invariants that should hold for
-- a mind.

data Mind s r c = Mind 
    {
     -- each hypothesis is exactly one of these
     irrefutable     :: HypothesisIDs,
     accepted        :: HypothesisIDs, 
     considering     :: HypothesisIDs, 
     refuted         :: HypothesisIDs,
     unacceptable    :: HypothesisIDs,
     -- other hypothesis states
     unevaluated     :: HypothesisIDs,
     confOverrides   :: HypothesisMap c,
     categories      :: HypothesisMap CategoryID,
     -- hypothesis evaluation
     aPriori         :: HypothesisMap (s -> c),
     confidence      :: HypothesisMap c,
     confBounds      :: HypothesisMap (c, c),
     beliefStatus    :: HypothesisMap Status,
     -- keep track of relationships between hypotheses
     effects         :: SubjectMap ObjectIDs, -- what should now be evaluated in the event of changes
     effectCounts    :: Map.Map (SubjectID, ObjectID) Int, -- reference counting for maintaining the effects field
     independent     :: AdjusterIDs,   -- adjusters that can not depend on other adjusters
     dependencies    :: [AdjusterIDs], -- consecutive sets of adjusters, in the order in which they should be followed
     -- backward and forward maps
     adjusts         :: SubjectMap AdjusterIDs,
     adjustersOf     :: ObjectMap AdjusterIDs,
     explains        :: SubjectMap ExplainsIDs,
     explainersOf    :: ObjectMap ExplainsIDs,
     constrainersOf  :: ObjectMap ConstrainerIDs,
     -- activity trace
     history         :: Maybe (Seq.Seq (MindEvent c)),
     -- repositories of relationships between hypotheses
     adjusters       :: AdjusterMap (SubjectID, ObjectID, AdjusterAction c),
     explainers      :: ExplainsMap (SubjectID, ObjectID),
     constrainers    :: ConstrainerMap (SubjectIDs, ObjectID, ConstrainerAction c),
     -- abductive state
     booster         :: c -> [c] -> c,
     transitive      :: TransitiveExplains,
     -- change of belief about hypotheses
     suggester       :: s -> (c, c) -> (r, Status),
     suggestions     :: HypothesisMap (r, Status)
    }

{-| 'ReasonerSettings' configures the behavior of 'reason'. -}

data ReasonerSettings = ReasonerSettings
    { 
     startFromScratch :: Bool {- ^ Reconsider all hypotheses; useful when
                                   situation has changed -}
    } deriving (Eq, Show)

-- Entirely internal data structures

{-| This is how an adjuster relation between hypothesis acts. It can look up
    the status of arbitrary hypotheses - actually, it looks up its subject(s)
    - and adjust the confidence of the object hypothesis accordingly. -}

newtype AdjusterAction c = AdjusterAction
    ((SubjectID -> (c, Status)) -> c -> c)

{-| This is how a constrainer relation between hypothesis acts. It can look up
    the status of arbitrary hypotheses - actually, it looks up its subject(s)
    - and adjust the confidence bounds of the object hypothesis accordingly. -}

newtype ConstrainerAction c = ConstrainerAction
    ((SubjectID -> (c, Status)) -> (c, c) -> (c, c))

-- Utility

{-| 'makeFunctionCache' take a function that in recursing expects to be able
    to refer to a memo of its own results, creates that memo, and partially
    applies the function to that memo. It is used in conjunction with
    'explainsLookup'. -}

makeFunctionCache :: forall a. (HypothesisMap a -> HypothesisID -> a) -> HypothesisIDs -> HypothesisID -> a

makeFunctionCache f domain =
    let memo = IDMap.fromDistinctAscList [ (x, f memo x) | x <- IDSet.toList domain ] :: HypothesisMap a
    in f memo

{-| 'explainsLookup' is a general-purpose function for traversing \'explains\'
    links, perhaps transitively, collating results as it goes. It expects to
    be able to use a memo of its own results. -}

explainsLookup :: forall a s r c.
                  Bool {- ^ If we should go up or down the explanatory links -}
               -> (HypothesisIDs -> HypothesisIDs) {- ^ A filter to select the nodes 
                                                        whose links we should follow -}
               -> (HypothesisIDs -> a) {- ^ Collate results from the nodes at one level -}
               -> ([a] -> a)           {- ^ Collate results from multiple levels -}
               -> Mind s r c           {- ^ The mind that contains the links of interest -}
               -> HypothesisMap a      {- ^ The results cache we are to use -}
               -> HypothesisID         {- ^ In the current call of this, 
                                            the node to start from -}
               -> a                    {- ^ The collated results -}

explainsLookup up selectRelevant reportOn combineLevels mind cache hypothesis =
    let explainsLinks = IDSet.toList (getSetFromMap ((if up then explainersOf else explains) mind) hypothesis) :: [ExplainsID]
        allTargets = IDSet.fromList (map ((if up then fst else snd) . getItemFromMap (explainers mind)) explainsLinks) :: HypothesisIDs
        relevantTargets = selectRelevant allTargets :: HypothesisIDs
        thisLevel = reportOn allTargets :: a
        sparseRetriever = explainsLookup up selectRelevant reportOn combineLevels mind cache :: HypothesisID -> a
        denseRetriever = getItemFromMap cache :: HypothesisID -> a
    in case transitive mind of
         NoTransitive -> thisLevel
         SparseTransitive -> combineLevels (thisLevel : map sparseRetriever (IDSet.toList relevantTargets))
         DenseTransitive  -> combineLevels (thisLevel : map denseRetriever  (IDSet.toList relevantTargets))

-- Interrogate

{-| 'showMind' returns a list of lines of human-readable description of the
    belief status of each of the hypothesis in a mind. -}

showMind :: forall s r c.
            Mind s r c {- ^ The mind whose state is to be described -}
         -> [String]   {- ^ The multi-line description of the mind's state -}

showMind mind =
    map report [(" irrefutable", irrefutable),
                ("    accepted", accepted),
                (" considering", considering),
                ("     refuted", refuted),
                ("unacceptable", unacceptable)]
    where
      report :: (String, Mind s r c -> HypothesisIDs) -> String
      report (name, field) =
          name ++ ": " ++ unwords (map show (IDSet.toList (field mind)))

{-| 'showHypothesis' returns a list of lines of human-readable description of
    a hypothesis' state. -}

showHypothesis :: Metric c 
               => HypothesisID {- ^ The ID number of the hypothesis of interest -}
               -> Mind s r c   {- ^ The mind that contains the hypothesis of interest -}
               -> [String]     {- ^ The multi-line description of the hypothesis' state -}

showHypothesis hypothesis mind =
    ["        hypothesis ID: " ++ show hypothesis,
     "             category: " ++ show (getItemFromMap (categories mind) hypothesis),
     "        belief status: " ++ show (getItemFromMap (beliefStatus mind) hypothesis),
     " intrinsic confidence: " ++ (ifAny $ IDMap.lookup hypothesis (confidence mind) >>= return . show),
     " accepting confidence: " ++ (ifAny $ IDMap.lookup hypothesis (confBounds mind) >>= return . show . fst),
     " rejection confidence: " ++ (ifAny $ IDMap.lookup hypothesis (confBounds mind) >>= return . show . snd),
     "overriding confidence: " ++ (ifAny $ IDMap.lookup hypothesis (confOverrides mind) >>= return . show)]
    where
      ifAny :: Maybe String -> String
      ifAny = fromMaybe "(none assigned)"

{-| 'hypothesisStatus' returns the current status of a hypothesis. -}

-- TODO: Possibly we shouldn't bother returning its category.

hypothesisStatus :: HypothesisID  {- ^ The ID number of the hypothesis of interest -}
                 -> Mind s r c    {- ^ The mind that contains the hypothesis of interest -}
                 -> (CategoryID, Maybe (c, c), Maybe c, Status) {- ^ The ID
                                             number of the category that the
                                             hypothesis is in; the lowest
                                             confidence the hypothesis is
                                             inferred to possibly have; the
                                             highest confidence the hypothesis
                                             is inferred to possibly have; the
                                             confidence override that is being
                                             applied; the belief status of the
                                             hypothesis. -}

hypothesisStatus hypothesis mind =
    (getItemFromMap (categories mind) hypothesis,
     IDMap.lookup hypothesis (confBounds mind),
     IDMap.lookup hypothesis (confOverrides mind),
     getItemFromMap (beliefStatus mind) hypothesis)

{-| 'unexplained' returns a set of the ID numbers of the accepted hypotheses
    that do not have a explanation for them accepted, but that are the object
    of some explains link. If such processing was enabled, then this set does
    not include IDs for hypotheses that were explained transitively in
    ambiguous situations through multiple paths. It lists categories because
    some hypotheses may have explainers in some contrast categories but not
    others. -}

-- 'unexplained' works by finding all the believed hypotheses that are the
-- subject of an explains link, finding all their not-disbelieved explanations
-- (transitively if requested) (per category), and reporting those that have
-- no believed explanations.

unexplained :: Mind s r c    {- ^ The mind that contains the hypothesis of interest -}
            -> [(CategoryID, HypothesisID)] {- ^ The set of unexplained hypotheses -}

unexplained mind =
    [ (category, object) | object <- IDSet.toList (believedOf allObjects) :: [ObjectID],
                           (category, explainers) <- IDMap.toList (possibleExplanations object) :: [(CategoryID, SubjectIDs)],
                           IDSet.null (believedOf explainers) ]
    where
      allObjects = IDMap.keysSet (explains mind) :: HypothesisIDs
      objectsIn  = IDSet.intersection allObjects :: HypothesisIDs -> ObjectIDs
      (believed,       believedOf) = (mappend (irrefutable mind) (accepted mind), IDSet.intersection believed)       :: (HypothesisIDs, HypothesisIDs -> HypothesisIDs)
      (notDisbelieved, notDisbelievedOf) = (mappend believed (considering mind),  IDSet.intersection notDisbelieved) :: (HypothesisIDs, HypothesisIDs -> HypothesisIDs)
      possibleExplanations = makeFunctionCache possibleExplanationsLookup (notDisbelievedOf allObjects) :: ObjectID -> CategoryMap SubjectIDs
      possibleExplanationsLookup :: ObjectMap (CategoryMap SubjectIDs) -> ObjectID -> CategoryMap SubjectIDs
      possibleExplanationsLookup cache =
          explainsLookup True (objectsIn . notDisbelievedOf) (reportExplanations (getItemFromMap (categories mind))) (IDMap.unionsWith mappend) mind cache
      reportExplanations :: (HypothesisID -> CategoryID) -> SubjectIDs -> CategoryMap SubjectIDs
      reportExplanations getCategory subjects = 
          IDMap.fromListWith mappend [ (getCategory subject, IDSet.singleton subject) | subject <- IDSet.toList subjects :: [SubjectID] ]

{-| 'mindTrace' returns a chronologically-ordered trace of the reasoning
    events in a mind's history. -}

mindTrace :: Mind s r c {- ^ The mind whose history is of interest -}
          -> Seq.Seq (MindEvent c) {- ^ The trace of the reasoning events
                                        since the mind was created -}

mindTrace mind =
    case history mind of
      Nothing -> Seq.empty
      Just history -> history

{-| 'hypothesisTrace' returns a chronologically-ordered trace of the reasoning
    events in a hypothesis's history. It is selected from what 'mindTrace'
    provides. -}

hypothesisTrace :: HypothesisID {- ^ The hypothesis whose history is of interest -}
                -> Mind s r c {- ^ The mind that holds the hypothesis that is being queried -}
                -> Seq.Seq (HypEvent c) {- ^ The trace of the reasoning events
                                             since the hypothesis was inserted
                                             into the mind -}

hypothesisTrace hypothesis mind = 
    Seq.fromList (mapMaybe considerEvent (Foldable.toList (mindTrace mind)))
    where
      considerEvent :: MindEvent c -> Maybe (HypEvent c)
      considerEvent event =
          case event of
            ME_HypothesisEvent eventHypothesis eventType ->
                if hypothesis == eventHypothesis then Just eventType else Nothing
            ME_HypothesesEvent eventHypotheses eventType ->
                if IDSet.member hypothesis eventHypotheses then Just eventType else Nothing
            _ -> Nothing

{-| 'setTrace' clears the mind's history and starts or stops tracing it. -}

setTrace :: Bool       {-^ If tracing should be done henceforth. -}
         -> Mind s r c {-^ A mind whose history should be cleared. -}
         -> Mind s r c {-^ The mind with its history cleared and further
                           tracing set accordingly. -}

setTrace trace mind =
    mind { history = if trace then Just Seq.empty else Nothing }

{-| 'unacceptableHypotheses' returns the set of hypotheses that are currently
    marked by 'setCounterfactual' as being counterfactual. -}

unacceptableHypotheses :: Mind s r c    {- ^ The mind whose hypotheses are being queried -}
                       -> HypothesisIDs {- ^ The current set of counterfactual hypotheses -}

unacceptableHypotheses = unacceptable

{-| 'refutedHypotheses' returns the set of hypotheses that the reasoner has
    decided to disbelieve. -}

refutedHypotheses :: Mind s r c {- ^ The mind whose hypotheses are being queried -}
                  -> HypothesisIDs {- ^ The current set of counterfactual hypotheses -}

refutedHypotheses = refuted

{-| 'consideringHypotheses' returns the set of hypotheses that the reasoner
    is undecided about. -}

consideringHypotheses :: Mind s r c    {- ^ The mind whose hypotheses are being queried -}
                      -> HypothesisIDs {- ^ The current set of hypotheses about which the
                                            reasoner is undecided -}

consideringHypotheses = considering

{-| 'acceptedHypotheses' returns the set of hypotheses that the reasoner has
    decided to believe. -}

acceptedHypotheses :: Mind s r c    {- ^ The mind whose hypotheses are being queried -}                      
                   -> HypothesisIDs {- ^ The current set of believed hypotheses -}

acceptedHypotheses = accepted

{-| 'irrefutableHypotheses' returns the set of hypotheses that are currently
    marked by 'setFactual' as being factual. -}

irrefutableHypotheses :: Mind s r c    {- ^ The mind whose hypotheses are being queried -}
                      -> HypothesisIDs {- ^ The current set of factual hypotheses -}

irrefutableHypotheses = irrefutable

{-| 'getAdjusters' returns all adjusting relations that are in a mind. -}

getAdjusters :: Mind s r c {- ^ The mind whose adjusting relations are being queried -}
             -> [(AdjusterID, SubjectID, ObjectID)] {- ^ All adjusting relations -}

getAdjusters mind =
    [ (adjuster, subject, object) | (adjuster, (subject, object, _)) <- IDMap.toList (adjusters mind) ]

{-| 'getConstrainers' returns all constraining relations that are in a mind. -}

getConstrainers :: Mind s r c {- ^ The mind whose adjusting relations are being queried -}
                -> [(ConstrainerID, SubjectIDs, ObjectID)] {- ^ All constraining relations -}

getConstrainers mind =
    [ (constrainer, subjects, object) | (constrainer, (subjects, object, _)) <- IDMap.toList (constrainers mind) ]

{-| 'getExplainers' returns all explains relations that are in a mind. -}

getExplainers :: Mind s r c {- ^ The mind whose explains relations are being queried -}
              -> [(ExplainsID, SubjectID, ObjectID)] {- ^ All explains relations -}

getExplainers mind =
    [ (explainer, subject, object) | (explainer, (subject, object)) <- IDMap.toList (explainers mind) ]

-- Change

-- Mutating data structures

{-| 'noteEvent' appends an event to a mind's history log -}

noteEvent :: MindEvent c -> Mind s r c -> Mind s r c

noteEvent newEvent mind =
    case history mind of
      Nothing -> mind
      Just oldEvents -> mind { history = Just (oldEvents Seq.|> newEvent) }

{-| 'noteEvents' appends events to a mind's history log -}

noteEvents :: [MindEvent c] -> Mind s r c -> Mind s r c

noteEvents newEvents mind =
    case history mind of
      Nothing -> mind
      Just oldEvents -> mind { history = Just (oldEvents Seq.>< Seq.fromList newEvents) }

{-| 'newMind' provides a new, empty mind, configured according to the supplied
    arguments and with history tracing not turned on. -}

newMind :: (Metric c, Ord r)
        => (c -> [c] -> c) {- ^ The confidence-boosting function: the
                           confidence of the best explanation; the confidences
                           of the rivals in descending order; the new boosted
                           confidence of the best explanation -}
        -> (s -> (c, c) -> (r, Status)) {- ^ The belief-status-suggesting
                                             function: the current situation;
                                             the lowest confidence the
                                             hypothesis is inferred to
                                             possibly have; the highest
                                             confidence the hypothesis is
                                             inferred to possibly have; the
                                             risk of that belief status being
                                             a poor choice; the suggested
                                             belief of the hypothesis -}
        -> TransitiveExplains {- ^ If this mind should follow explanatory
                                   relations transitively in finding abductive
                                   differentials and, if so, if it should
                                   cache results -}
        -> Mind s r c {- ^ The new, empty mind, configured as specified -}

newMind boosterFunction suggesterFunction transitiveExplains =
    Mind mempty mempty mempty mempty mempty
         mempty IDMap.empty IDMap.empty
         IDMap.empty IDMap.empty IDMap.empty IDMap.empty
         IDMap.empty Map.empty mempty []
         IDMap.empty IDMap.empty IDMap.empty IDMap.empty IDMap.empty
         Nothing
         IDMap.empty IDMap.empty IDMap.empty
         boosterFunction transitiveExplains
         suggesterFunction IDMap.empty

{-| 'addHypothesis' adds a given elementary hypothesis to a mind, putting it
    in the given category. -}

addHypothesis :: (Metric c, Ord r) 
              => HypothesisID {- ^ A unique ID number for identifying the hypothesis -}
              -> CategoryID   {- ^ The ID number of the category that the hypothesis is in -}
              -> (s -> c)     {- ^ A function for calculating the hypothesis'
                                   a priori confidence from the current situation -}
              -> Mind s r c   {- ^ The mind to which the hypothesis is to be added -}
              -> Mind s r c   {- ^ The mind with the given hypothesis added -}

addHypothesis hypothesis category hypAPriori mind =
    noteEvents (ME_AddHypothesis hypothesis :
                map (ME_HypothesisEvent hypothesis)
                    [HE_CategorySet category,
                     HE_BeliefStatusSet Consider,
                     HE_NeedsEvaluation]) $
    mind { considering = IDSet.insert hypothesis (considering mind),
           unevaluated = IDSet.insert hypothesis (unevaluated mind),
           aPriori = IDMap.insertNew hypothesis hypAPriori (aPriori mind),
           beliefStatus = IDMap.insert hypothesis Consider (beliefStatus mind),
           categories = IDMap.insert hypothesis category (categories mind) }

{-| 'setCategory' sets the category of a hypothesis in a mind. -}

setCategory :: (Metric c, Ord r) 
            => HypothesisID {- ^ The ID number of the hypothesis whose category is to be set -}
            -> CategoryID   {- ^ The ID number of the new category for the hypothesis -}
            -> Mind s r c   {- ^ The mind that has the hypothesis whose category is to be set -}
            -> Mind s r c   {- ^ The mind with the given hypothesis' category set -}

setCategory hypothesis category mind =
    noteEvents (map (ME_HypothesisEvent hypothesis)
                    [HE_CategorySet category,
                     HE_NeedsEvaluation]) $
    mind { unevaluated = IDSet.insert hypothesis (unevaluated mind),
           categories = IDMap.insert hypothesis category (categories mind) }

{-| 'removeHypothesis' removes a hypothesis from a mind. It is not an error if
    the hypothesis did not exist in the mind. -}

removeHypothesis :: HypothesisID {- ^ The ID number of the hypothesis that is to be removed -}
                 -> Mind s r c   {- ^ The mind from which the hypothesis is to be removed -}
                 -> Mind s r c   {- ^ The mind with the given hypothesis and the hypothesis' relations removed -}

removeHypothesis hypothesis mind1 =
    let adjustersToRemove    = [ adjuster    | (adjuster, (subject, object, _)) <- IDMap.toList (adjusters mind1),
                                               subject == hypothesis || object == hypothesis ] :: [AdjusterID]
        constrainersToRemove = [ constrainer | (constrainer, (subjects, object, _)) <- IDMap.toList (constrainers mind1),
                                               IDSet.member hypothesis subjects || object == hypothesis ] :: [ConstrainerID]
        explainersToRemove   = [ explainer   | (explainer, (subject, object)) <- IDMap.toList (explainers mind1),
                                               subject == hypothesis || object == hypothesis ] :: [ExplainsID]
        mind2 = foldl' (flip removeAdjuster)    mind1 adjustersToRemove
        mind3 = foldl' (flip removeConstrainer) mind2 constrainersToRemove
        mind4 = foldl' (flip removeExplains)    mind3 explainersToRemove
        mind5 = mind4 { unacceptable  = IDSet.delete hypothesis (unacceptable mind4),
                        refuted       = IDSet.delete hypothesis (refuted mind4),
                        considering   = IDSet.delete hypothesis (considering mind4),
                        accepted      = IDSet.delete hypothesis (accepted mind4),
                        irrefutable   = IDSet.delete hypothesis (irrefutable mind4),
                        unevaluated   = IDSet.delete hypothesis (unevaluated mind4),
                        confOverrides = IDMap.delete hypothesis (confOverrides mind4),
                        aPriori       = IDMap.delete hypothesis (aPriori mind4),
                        confidence    = IDMap.delete hypothesis (confidence mind4),
                        confBounds    = IDMap.delete hypothesis (confBounds mind4),
                        beliefStatus  = IDMap.delete hypothesis (beliefStatus mind4),
                        categories    = IDMap.delete hypothesis (categories mind4)
                      }
    in noteEvent (ME_RemoveHypothesis hypothesis) mind5

{-| 'addEffect' adds an effect to the mind's effects cache. -}

addEffect :: SubjectID -> ObjectID -> Mind s r c -> Mind s r c

addEffect subject object mind =
    let mind' = noteEvent (ME_HypothesisEvent object HE_NeedsEvaluation) $
                mind { unevaluated = IDSet.insert object (unevaluated mind) }
    in if Map.member (subject, object) (effectCounts mind)
       then mind' { effectCounts = Map.adjust succ (subject, object) (effectCounts mind) }
       else mind' { effectCounts = Map.insert (subject, object) 1 (effectCounts mind),
                    effects = IDMap.insertWith mappend subject (IDSet.singleton object) (effects mind) }

{-| 'removeEffect' removes an effect from the mind's effects cache. -}

removeEffect :: SubjectID -> ObjectID -> Mind s r c -> Mind s r c

removeEffect subject object mind =
    let mind' = noteEvent (ME_HypothesisEvent object HE_NeedsEvaluation) $
                mind { unevaluated = IDSet.insert object (unevaluated mind) }
    in if getItemFromMap (effectCounts mind) (subject, object) > 1
       then mind' { effectCounts = Map.adjust pred (subject, object) (effectCounts mind) }
       else mind' { effectCounts = Map.delete (subject, object) (effectCounts mind),
                    effects = removeFromMapSet (effects mind) subject object }
    where
      -- this shadows the usual getItemFromMap that works on IDMaps
      getItemFromMap :: (Ord k, Show k) => Map.Map k a -> k -> a
      getItemFromMap mp key =
          Map.findWithDefault err key mp
          where
            err = error ("failed to find " ++ show key)

{-| 'addDependency' incrementally adds a dependency to the ordered list of the
    order in which we must process adjusting relations. It is used by
    'addAdjuster'. -}

addDependency :: AdjusterID -> Mind s r c -> Mind s r c

addDependency adjuster mind =
    let (subject, object, _) = getItemFromMap (adjusters mind) adjuster
        -- find out which adjusters affect the subject: a ? -> S relation must precede our S -> O relation
        mustPrecede = getSetFromMap (adjustersOf mind) subject :: AdjusterIDs
        -- find out which adjusters are affected by the object: a O -> ? relation must follow our S -> O relation
        mustFollow  = getSetFromMap (adjusts mind) object :: AdjusterIDs
        -- beforeMustFollow: which adjuster groups precede any O -> ? relation
        -- afterMustFollow: adjuster groups that have or follow a O -> ? relation
        (beforeMustFollow, afterMustFollow) = 
            if IDSet.null mustFollow
            then (dependencies mind, [])
            else span (IDSet.null . IDSet.intersection mustFollow) (dependencies mind) :: ([AdjusterIDs], [AdjusterIDs])
        -- toReAdd: the ? -> S relations that were placed with or after a O -> ? relation: we
        --   will take them out, insert our S -> O relation, then let them fit in around it
        -- followers: the relations with or after a O -> ? one, without any ? -> S relations
        (toReAdd, followers) =
            if IDSet.null mustPrecede
            then (mempty, afterMustFollow)
            else extractPreceders mustPrecede afterMustFollow :: (AdjusterIDs, [AdjusterIDs])
        -- so, we put our S -> O relation after any ? -> S relations,
        --  append the groups that have or follow a O -> ? relation,
        rejoined = addAdjusterAfter mustPrecede adjuster beforeMustFollow ++ followers
        --  then re-add the ? -> S relations that did not precede the first O -> ? one
    in IDSet.fold addDependency (mind { dependencies = rejoined }) toReAdd
    where
      -- we remove any ? -> S relations from the groups that have or follow a O -> ? relation
      extractPreceders :: AdjusterIDs -> [AdjusterIDs] -> (AdjusterIDs, [AdjusterIDs])
      extractPreceders toExtract dependencies =
          let (extracted, dependencies')= unzip (map (IDSet.partition (`IDSet.member` toExtract)) dependencies)
          in (mconcat extracted, filter (not . IDSet.null) dependencies')
      -- we add our S -> O relation to the groups that are before the first O -> ? relation
      addAdjusterAfter :: AdjusterIDs -> AdjusterID -> [AdjusterIDs] -> [AdjusterIDs]
      addAdjusterAfter _ adjuster [] = [IDSet.singleton adjuster]
      addAdjusterAfter mustPrecede adjuster [lastSet] = 
          -- if the last of those groups has no ? -> S relation,
          if IDSet.null (IDSet.intersection mustPrecede lastSet)
          -- we add the S -> O relation into there
          then [IDSet.insert adjuster lastSet]
          -- otherwise we append a new group and put it in that
          else [lastSet, IDSet.singleton adjuster]
      -- we add our S -> O relation to the group immediately preceding the first O -> relation
      addAdjusterAfter mustPrecede adjuster (dependencyGroup : dependencyGroups) = 
          dependencyGroup : addAdjusterAfter mustPrecede adjuster dependencyGroups

{-| 'removeDependency' removes a dependency from the list of the order in
    which we must process adjusting relations. It is used by 'removeAdjuster'.
    -}

removeDependency :: AdjusterID -> Mind s r c -> Mind s r c

removeDependency adjuster mind =
    mind { dependencies = filter (not . IDSet.null) (map (IDSet.delete adjuster) (dependencies mind)) }

{-| 'addAdjuster' adds an adjusting relation to a mind. It an error if the
    hypotheses to which it refers have not already been added to the mind with
    'addHypothesis'. The network of adjusting relations created using the
    Right constructor must be acyclic. -}

-- TODO: Add error-detection for cyclic adjustment.

addAdjuster :: forall s r c.
               AdjusterID {- ^ A unique ID number for identifying the adjusting relation -}
            -> SubjectID  {- ^ The ID number of the hypothesis that causes the adjusting -}
            -> Either (Maybe (c -> c), Maybe (c -> c))
                      (Maybe (c -> c -> c), Maybe (c -> c -> c))
               {- ^ The adjustment function. In the tuples: first, maybe one
                    for if the subject hypothesis is accepted; second, maybe
                    one for if the subject hypothesis is refuted. The function
                    arguments are: the confidence of the subject (only in
                    Right); the unadjusted confidence of the object; the
                    adjusted confidence of the object. -}
            -> ObjectID   {- ^ The ID number of the hypothesis that gets adjusted -}
            -> Mind s r c {- ^ The mind to which the adjusting relation is to be added -} 
            -> Mind s r c {- ^ The mind with the given adjusting relation added -}

addAdjuster adjuster subject functions object mind =
    noteEvent (ME_AddAdjuster adjuster subject object) $
    (either (const addIndependent) (const addDependency) functions) adjuster $
    addEffect subject object $
    mind { adjusts = IDMap.insertWith mappend subject (IDSet.singleton adjuster) (adjusts mind),
           adjustersOf = IDMap.insertWith mappend object (IDSet.singleton adjuster) (adjustersOf mind),
           adjusters = IDMap.insertNew adjuster (subject, object, AdjusterAction action) (adjusters mind) }
    where
      normalize ::  Either (Maybe (c -> c), Maybe (c -> c)) (Maybe (c -> c -> c), Maybe (c -> c -> c)) -> (Maybe (c -> c -> c), Maybe (c -> c -> c))
      normalize (Left (ifAccepted, ifRefuted)) =
          (liftM const ifAccepted, liftM const ifRefuted)
      normalize (Right f) = f
      action :: (SubjectID -> (c, Status)) -> c -> c
      action subjectDirectory priorConfidence =
          case (subjectDirectory subject, normalize functions) of
              ((subjectConfidence, Accept), (Just adjuster, _)) ->
                  adjuster subjectConfidence priorConfidence
              ((subjectConfidence, Refute), (_, Just adjuster)) ->
                  adjuster subjectConfidence priorConfidence
              _ -> priorConfidence
      addIndependent :: AdjusterID -> Mind s r c -> Mind s r c
      addIndependent adjuster mind =
          mind { independent = IDSet.insert adjuster (independent mind) }

{-| 'removeAdjuster' removes an adjusting relation from a mind. It is not an
    error if the hypothesis did not exist in the mind. -}

-- Note that before we remove a link we check to see if it's already not
-- there, and if it isn't then the remove function makes no changes. This is
-- partly because we don't want the reference counting of the Mind's
-- effectCounts field to go wrong. This pattern will be seen in further
-- functions below.

removeAdjuster :: forall s r c.
                  AdjusterID {- ^ The ID number of the adjusting relation that is to be removed -}
               -> Mind s r c {- ^ The mind from which the adjusting relation is to be removed -}
               -> Mind s r c {- ^ The mind with the given adjusting relation removed -}

removeAdjuster adjuster mind =
    let (subject, object, _) = getItemFromMap (adjusters mind) adjuster :: (SubjectID, ObjectID, AdjusterAction c)
    in if IDMap.notMember adjuster (adjusters mind) then mind
       else noteEvent (ME_RemoveAdjuster adjuster) $
            removeDependency adjuster $
            removeEffect subject object $
            mind { adjusts = removeFromMapSet (adjusts mind) subject adjuster,
                   adjustersOf = removeFromMapSet (adjustersOf mind) object adjuster,
                   adjusters = IDMap.delete adjuster (adjusters mind),
                   independent = IDSet.delete adjuster (independent mind) }

{-| 'addConstrainer' adds a constraining relation to a mind. It an error if
    the hypotheses to which it refers have not already been added to the mind
    with 'addHypothesis'. -}

addConstrainer :: forall s r c.
                  Metric c
               => ConstrainerID {- ^ A unique ID number for identifying the constraining relation -}
               -> SubjectIDs    {- ^ The ID numbers of the hypotheses that cause the constraining -}
               -> ([c] -> [Bound c]) {- ^ The constraining function, which
                                          bounds the object hypothesis'
                                          confidence based on the confidence
                                          of the subject hypotheses -}
               -> ObjectID      {- ^ The ID number of the hypothesis that gets constrained -}
               -> Mind s r c    {- ^ The mind to which the constraining relation is to be added -}
               -> Mind s r c    {- ^ The mind with the given constraining relation added -}

addConstrainer constrainer subjects function object mind =
    flip (IDSet.fold (`addEffect` object)) subjects $
    mind {constrainersOf = IDMap.insertWith mappend object (IDSet.singleton constrainer) (constrainersOf mind),
          constrainers = IDMap.insertNew constrainer (subjects, object, ConstrainerAction action) (constrainers mind) }
    where
      action :: (SubjectID -> (c, Status)) -> (c, c) -> (c, c)
      action subjectDirectory priorConfidence =
          let bounds = function (map (fst . subjectDirectory) (IDSet.toList subjects)) :: [Bound c]
          in foldl' processBound priorConfidence bounds
      processBound (lowestConfidence, highestConfidence) (HigherThan confidence) =
          (max lowestConfidence confidence, highestConfidence)
      processBound (lowestConfidence, highestConfidence) (LowerThan confidence) =
          (lowestConfidence, min highestConfidence confidence)
          
{-| 'removeConstrainer' removes a constraining relation from a mind that was
    set by 'addConstrainer'. It is not an error if the constraint did not
    exist in the mind. -}

removeConstrainer :: forall s r c.
                     ConstrainerID {- ^ The ID number of the constrainer choice set that is to be removed -}
                  -> Mind s r c    {- ^ The mind from which the constraining relation is to be removed -}
                  -> Mind s r c    {- ^ The mind with the given constraining relation removed -}

removeConstrainer constrainer mind =
    let (subjects, object, _) = getItemFromMap (constrainers mind) constrainer :: (SubjectIDs, ObjectID, ConstrainerAction c)
     in if IDMap.notMember constrainer (constrainers mind) then mind
        else flip (IDSet.fold (`removeEffect` object)) subjects $
             mind { constrainersOf = removeFromMapSet (constrainersOf mind) object constrainer,
                    constrainers = IDMap.delete constrainer (constrainers mind) }

{-| 'subjectRelations' finds the combinations of affecter-affected pairs that
    correspond to how explains relations cause the given subject to affect and
    be affected by confidence boosts that arise from abductive differentials.
    -}

-- TODO: This could be made sensitive to contrast categories. Then,
-- setCategory would have to alter the effects map.

subjectRelations :: SubjectID -> Mind s r c -> [(HypothesisID, HypothesisID)]

subjectRelations subject mind =
    let differentials =
            [ (object, explanations) | object <- IDSet.toList (possibleExplained subject) :: [ObjectID],
                                       let explanations = possibleExplanations object :: SubjectIDs,
                                       not (IDSet.null explanations) ] :: [(ObjectID, SubjectIDs)]
    in concat [ (object, subject) : concat [ [ (subject, rival), (rival, subject) ]
                                             | rival <- IDSet.toList (IDSet.delete subject subjects) ] 
                | (object, subjects) <- differentials ] :: [(HypothesisID, HypothesisID)]
    where
      (allSubjects, allObjects) = (IDMap.keysSet (explains mind), IDMap.keysSet (explainersOf mind)) :: (HypothesisIDs, HypothesisIDs)
      (subjectsIn, objectsIn) = (IDSet.intersection allSubjects, IDSet.intersection allObjects) :: (HypothesisIDs -> SubjectIDs, HypothesisIDs -> ObjectIDs)
      possibleExplained    = makeFunctionCache possibleExplainedLookup    allSubjects :: SubjectID -> ObjectIDs
      possibleExplanations = makeFunctionCache possibleExplanationsLookup allObjects  :: ObjectID -> SubjectIDs
      possibleExplainedLookup :: SubjectMap ObjectIDs -> SubjectID -> ObjectIDs
      possibleExplainedLookup cache = 
          explainsLookup False subjectsIn id mconcat mind cache
      possibleExplanationsLookup :: ObjectMap SubjectIDs -> ObjectID -> SubjectIDs
      possibleExplanationsLookup cache =
          explainsLookup True  objectsIn  id mconcat mind cache

{-| 'addExplains' adds an explains relation to a mind. It an error if the
    hypotheses to which it refers have not already been added to the mind with
    'addHypothesis'. -}

addExplains :: ExplainsID {- ^ A unique ID number for identifying the explains relation -}
            -> SubjectID  {- ^ The ID number of the hypothesis that is the explanation -}
            -> ObjectID   {- ^ The ID number of the hypothesis that is explained -}
            -> Mind s r c {- ^ The mind to which the explains relation is to be added -}
            -> Mind s r c {- ^ The mind with the given explains relation added -}

addExplains explainer subject object mind =
    let mindWithExplainer =
            noteEvent (ME_AddExplains explainer subject object) $
            mind { explains = IDMap.insertWith mappend subject (IDSet.singleton explainer) (explains mind),
                   explainersOf = IDMap.insertWith mappend object (IDSet.singleton explainer) (explainersOf mind),
                   explainers = IDMap.insertNew explainer (subject, object) (explainers mind) }
    in (foldr1 (.) (map (uncurry addEffect) (subjectRelations subject mindWithExplainer))) mindWithExplainer

{-| 'removeExplains' removes an explains relation from a mind. It is not an
    error if the explains relation did not exist in the mind. -}

removeExplains :: ExplainsID {- ^ The ID number of the explains relation that is to be removed -}
               -> Mind s r c {- ^ The mind from which the explains relation is to be removed -}
               -> Mind s r c {- ^ The mind with the given explains relation removed -}

removeExplains explainer mindWithExplainer =
    let (subject, object) = getItemFromMap (explainers mindWithExplainer) explainer :: (SubjectID, ObjectID)
    in if IDMap.notMember explainer (explainers mindWithExplainer) then mindWithExplainer
       else noteEvent (ME_RemoveExplains explainer) $
            (foldr1 (.) (map (uncurry removeEffect) (subjectRelations subject mindWithExplainer))) $
            mindWithExplainer { explains = removeFromMapSet (explains mindWithExplainer) subject explainer,
                                explainersOf = removeFromMapSet (explainersOf mindWithExplainer) object explainer,
                                explainers = IDMap.delete explainer (explainers mindWithExplainer) }

{-| 'setConfidence' overrides hypotheses' confidence so that in a mind they
    are locked to a given value. The underlying confidence is still calculated
    and logged but applies no further than that. -}

setConfidence :: forall s r c.
                 (Metric c, Ord r) 
              => HypothesisIDs {- ^ The ID numbers of the hypotheses whose confidence is to be set -}
              -> c             {- ^ The confidence value to which the hypotheses' confidence is to be set -}
              -> Mind s r c    {- ^ The mind that has the hypotheses whose confidence is to be set -}
              -> Mind s r c    {- ^ The mind with the given hypotheses' confidence set -}

setConfidence hypotheses confidenceValue mind =
    let newOverrides = IDMap.fromDistinctAscList (zip (IDSet.toList hypotheses) (repeat confidenceValue)) :: HypothesisMap c
    in noteEvents (map (ME_HypothesesEvent hypotheses)
                       [HE_ConfidenceOverride (Just confidenceValue),
                        HE_NeedsEvaluation]) $
       mind { confOverrides = IDMap.union newOverrides (confOverrides mind),
              unevaluated = mappend hypotheses (unevaluated mind) }

{-| 'unsetConfidence' reverses the effect of 'setConfidence', allowing
    'reason' to adjust hypotheses' confidence. It is not an error if
    'setConfidence' was not applied to the given hypotheses. -}

unsetConfidence :: (Metric c, Ord r)
                => HypothesisIDs {- ^ The ID numbers of the hypotheses whose confidence is to be unset -}
                -> Mind s r c    {- ^ The mind that has the hypotheses whose confidence is to be unset -}
                -> Mind s r c    {- ^ The mind with the given hypotheses' confidence unset -}

unsetConfidence hypotheses mind =
    noteEvents (map (ME_HypothesesEvent hypotheses) 
                     [HE_ConfidenceOverride Nothing,
                      HE_NeedsEvaluation]) $
    mind { confOverrides = IDSet.fold IDMap.delete (confOverrides mind) hypotheses,
           unevaluated = mappend hypotheses (unevaluated mind) }

{-| 'changeStatus' sets or unsets a belief status override for a set of
    hypotheses. It marks the hypotheses as needing evaluation. -}

changeStatus :: (Metric c, Ord r) => (HypothesisIDs -> Mind s r c -> Mind s r c) -> Status -> HypothesisIDs -> Mind s r c -> Mind s r c

changeStatus mindChanger status hypotheses mind0 =
    let mind1 = removeHypothesesFromTypes mind0
        mind2 = mindChanger hypotheses mind1
        mind3 = IDSet.fold setBeliefStatus mind2 hypotheses
    in
    noteEvents (map (ME_HypothesesEvent hypotheses) 
                     [HE_BeliefStatusOverride (if status == Consider then Nothing else Just status),
                      HE_NeedsEvaluation])
               mind3
    where
      removeHypothesesFromTypes :: Mind s r c -> Mind s r c
      removeHypothesesFromTypes mind =
          mind { unacceptable = IDSet.difference (unacceptable mind) hypotheses,
                 refuted      = IDSet.difference (refuted      mind) hypotheses,
                 considering  = IDSet.difference (considering  mind) hypotheses,
                 accepted     = IDSet.difference (accepted     mind) hypotheses,
                 irrefutable  = IDSet.difference (irrefutable  mind) hypotheses,
                 unevaluated  = mappend hypotheses (unevaluated mind) }
      setBeliefStatus :: HypothesisID -> Mind s r c -> Mind s r c
      setBeliefStatus hypothesis mind =
          mind { beliefStatus = IDMap.insert hypothesis status (beliefStatus mind) }

{-| 'setFactual' requires of a mind that it believe the given hypotheses.
    Their confidence is not automatically set to be high so it may often be
    appropriate to use 'setConfidence' in conjunction with 'setFactual'. -}

setFactual :: (Metric c, Ord r) 
           => HypothesisIDs {- ^ The ID numbers of the hypotheses that must be accepted -}
           -> Mind s r c    {- ^ The mind that has the hypotheses that must be accepted -}
           -> Mind s r c    {- ^ The mind with the given hypotheses accepted -}

setFactual =
    changeStatus mindChanger Accept
    where
      mindChanger :: HypothesisIDs -> Mind s r c -> Mind s r c
      mindChanger hypotheses mind = mind { irrefutable = mappend hypotheses (irrefutable mind),
                                           suggestions = IDSet.fold IDMap.delete (suggestions mind) hypotheses }

{-| 'setCounterfactual' requires of a mind that it disbelieve the given
    hypotheses. Their confidence is not automatically set to be low so it may
    often be appropriate to use 'setConfidence' in conjunction with
    'setCounterfactual'. -}

setCounterfactual :: (Metric c, Ord r) 
                  => HypothesisIDs {- ^ The ID numbers of the hypotheses that must be refuted -}
                  -> Mind s r c    {- ^ The mind that has the hypotheses that must be refuted -}
                  -> Mind s r c    {- ^ The mind with the given hypotheses refuted -}

setCounterfactual =
    changeStatus mindChanger Refute
    where
      mindChanger :: HypothesisIDs -> Mind s r c -> Mind s r c
      mindChanger hypotheses mind = mind { unacceptable = mappend hypotheses (unacceptable mind),
                                           suggestions = IDSet.fold IDMap.delete (suggestions mind) hypotheses }

{-| 'unsetStatus' reverses the effects of 'setFactual' and 'setCounterfactual'
    to put the belief status of the given hypotheses back into the hands of
    'reason' with the hypothesis status as undecided. It is not an error if
    those were not applied to the given hypotheses. -}

unsetStatus :: (Metric c, Ord r)
            => HypothesisIDs {- ^ The ID numbers of the hypotheses whose belief status 'reason' must be allowed to adjust -}
            -> Mind s r c    {- ^ The mind that has the hypotheses whose belief status 'reason' must be allowed to adjust -}
            -> Mind s r c    {- ^ The mind with the given hypotheses adjustable by 'reason'  -}

unsetStatus =
    changeStatus mindChanger Consider
    where
      mindChanger :: HypothesisIDs -> Mind s r c -> Mind s r c
      mindChanger hypotheses mind = mind { considering = mappend hypotheses (considering mind) }

{-| 'transitiveClosureSteps' is used by 'calculateDependencies' to find a safe
    order in which to process adjusting relations so that if one hypothesis
    adjusts another, the former's confidence can be calculated before the
    latter's. -}

transitiveClosureSteps :: (HypothesisID -> HypothesisIDs) -> HypothesisIDs -> [HypothesisIDs]

transitiveClosureSteps followStep unadjustedHypotheses =
    followAllSteps [] mempty unadjustedHypotheses
    where
      followAllSteps :: [HypothesisIDs] -> HypothesisIDs -> HypothesisIDs -> [HypothesisIDs]
      followAllSteps journey followed unfollowed =
          if IDSet.null unfollowed then journey
          else let unfollowedLeadTo = mconcat (map followStep (IDSet.toList unfollowed)) :: HypothesisIDs
                   nowFollowed = mappend followed unfollowed :: HypothesisIDs
                   newUnfollowed = IDSet.difference unfollowedLeadTo nowFollowed :: HypothesisIDs
               in followAllSteps (journey ++ [unfollowed]) nowFollowed newUnfollowed

{-| 'calculateDependencies' recalculates the order in which to process
    adjusting relations. This function should not change the behavior of the
    system, but it may allow subsequent operations to proceed faster than
    before. -}

-- To find which order it is safe to apply adjusting relations in, we start
-- from the unadjusted hypotheses and use 'transitiveClosureSteps' to follow
-- the adjusting relations step by step en masse, noting which set of
-- hypotheses we got to in each step and not revisiting any hypotheses. We
-- suggest the list of the adjusters of each of these sets as the basis for
-- ordering application of adjusting relations.

calculateDependencies :: Mind s r c {- ^ The mind whose dependencies may be inefficient -}
                      -> Mind s r c {- ^ A mind with recalculated dependencies -}

calculateDependencies mind =
    mind { dependencies = map adjustersOfAll (transitiveClosureSteps nextAdjusts unadjusted) }
    where
      allHypotheses = mconcat (map ($ mind) [unacceptable, refuted, considering, accepted, irrefutable]) :: HypothesisIDs
      unadjusted = IDSet.difference allHypotheses (IDMap.keysSet (adjustersOf mind)) :: HypothesisIDs
      nextAdjusts :: SubjectID -> ObjectIDs
      nextAdjusts = getSetFromMap $ IDMap.fromListWith mappend [ (subject, IDSet.singleton object) | (subject, object, _) <- IDMap.elems (adjusters mind) ]
      adjustersOfAll :: HypothesisIDs -> AdjusterIDs
      adjustersOfAll hypotheses = mconcat (map (getSetFromMap (adjustersOf mind)) (IDSet.toList hypotheses))

-- Reason

{-| 'hypothesisConfidence' looks up the given hypothesis' confidence in the
    mind. -}

hypothesisConfidence :: forall s r c. Mind s r c -> HypothesisID -> c

hypothesisConfidence mind hypothesis =
    let underlyingConfidence = getItemFromMap (confidence mind) hypothesis :: c
    in IDMap.findWithDefault underlyingConfidence hypothesis (confOverrides mind)

{-| 'hypothesisOpinion' looks up a mind's opinion of the given hypothesis. -}

hypothesisOpinion :: Mind s r c -> HypothesisID -> (c, Status)

hypothesisOpinion mind hypothesis =
    (hypothesisConfidence mind hypothesis, getItemFromMap (beliefStatus mind) hypothesis)

{-| 'resetConfidences' sets hypotheses' confidence to their a priori value. -}

resetConfidences :: forall s r c. Mind s r c -> s -> HypothesisIDs -> Mind s r c

resetConfidences mind situation hypotheses =
    IDSet.fold resetConfidence mind hypotheses
    where
      resetConfidence :: HypothesisID -> Mind s r c -> Mind s r c
      resetConfidence hypothesis mind' =
          let resetValue = (getItemFromMap (aPriori mind') hypothesis) situation :: c
          in noteEvent (ME_HypothesisEvent hypothesis (HE_APrioriConfidence resetValue)) $
             mind' { confidence = IDMap.insert hypothesis resetValue (confidence mind') }

{-| 'applyAdjusters' applies the given adjusting relations in the given order.
    -}

-- It would be nice to find a way to avoid searching twice through the
-- confidence map.

applyAdjusters :: forall s r c. Mind s r c -> [AdjusterIDs] -> Mind s r c

applyAdjusters mind adjusterSets =
    foldl' applyAdjuster mind (concatMap IDSet.toList adjusterSets)
    where
      applyAdjuster :: Mind s r c -> AdjusterID -> Mind s r c
      applyAdjuster mind adjuster =
          let (_, object, AdjusterAction action) = getItemFromMap (adjusters mind) adjuster :: (SubjectID, ObjectID, AdjusterAction c)
              mind' = mind { confidence = IDMap.adjust (action (hypothesisOpinion mind)) object (confidence mind) }
          in noteEvent (ME_HypothesisEvent object (HE_ConfidenceAdjustment adjuster (getItemFromMap (confidence mind') object))) $ 
             mind'

{-| 'findActiveDifferentials' traverses explains links to find abductive
    differentials. It uses hypotheses' belief status to limit the
    differentials to those that currently apply. -}

findActiveDifferentials :: Mind s r c -> HypothesisIDs -> [(ObjectID, SubjectIDs)]

findActiveDifferentials mind startingSubjects =
    [ (object, subjects) 
      | object <- IDSet.toList (mconcat (map possibleExplained (IDSet.toList (subjectsIn startingSubjects)))) :: [ObjectID],
        subjects <- IDMap.elems (possibleExplanations object) :: [SubjectIDs] ]
    where
      (believed,       believedOf) = (mappend (irrefutable mind) (accepted mind), IDSet.intersection believed)       :: (HypothesisIDs, HypothesisIDs -> HypothesisIDs)
      (notDisbelieved, notDisbelievedOf) = (mappend believed (considering mind),  IDSet.intersection notDisbelieved) :: (HypothesisIDs, HypothesisIDs -> HypothesisIDs)
      (allSubjects, allObjects) = (IDMap.keysSet (explains mind), IDMap.keysSet (explainersOf mind)) :: (HypothesisIDs, HypothesisIDs)
      (subjectsIn, objectsIn) = (IDSet.intersection allSubjects, IDSet.intersection allObjects) :: (HypothesisIDs -> SubjectIDs, HypothesisIDs -> ObjectIDs)
      possibleExplained    = makeFunctionCache possibleExplainedLookup    (subjectsIn (considering mind)) :: SubjectID -> ObjectIDs
      possibleExplanations = makeFunctionCache possibleExplanationsLookup (notDisbelievedOf allObjects)   :: ObjectID -> CategoryMap SubjectIDs
      possibleExplainedLookup :: SubjectMap ObjectIDs -> SubjectID -> ObjectIDs
      possibleExplainedLookup cache = 
          explainsLookup False (subjectsIn . IDSet.intersection (considering mind)) believedOf mconcat mind cache
      possibleExplanationsLookup :: ObjectMap (CategoryMap SubjectIDs) -> ObjectID -> CategoryMap SubjectIDs
      possibleExplanationsLookup cache =
          explainsLookup True (objectsIn . notDisbelievedOf) (reportExplanations (getItemFromMap (categories mind))) (IDMap.unionsWith mappend) mind cache
      reportExplanations :: (HypothesisID -> CategoryID) -> SubjectIDs -> CategoryMap SubjectIDs
      reportExplanations getCategory subjects = 
          IDMap.fromListWith mappend [ (getCategory subject, IDSet.singleton subject) | subject <- IDSet.toList subjects :: [SubjectID] ]

{-| 'processDifferential' applies a best-explanation boost where appropriate
    for the given abductive differential. Note that a confidence override can
    cause a hypothesis to become the best explanation, but that it is its
    underlying confidence that receives the boost. So, underlying confidences
    may not reflect what the actual confidence would be without the override.
    It only adjusts the confidence of the \'permitted\' hypotheses that are
    currently being evaluated. -}

processDifferential :: forall s r c. Metric c => SubjectIDs -> Mind s r c -> ObjectID -> SubjectIDs -> Mind s r c

processDifferential permitted mind explained explainers =
    let explainersWithConfidence = map appendConfidence (IDSet.toList explainers) :: [(SubjectID, c)]
        (bestExplanation, bestConfidence) : remainder = sortBy (on (flip compare) snd) explainersWithConfidence
        boostedConfidence = (booster mind) (getItemFromMap (confidence mind) bestExplanation) (map snd remainder)
     in if IDSet.null explainers || IDSet.notMember bestExplanation permitted
        then mind
        else noteEvent (ME_HypothesisEvent bestExplanation (HE_ConfidenceBoost explained boostedConfidence)) $
             mind { confidence = IDMap.insert bestExplanation boostedConfidence (confidence mind) }
    where
      appendConfidence :: SubjectID -> (SubjectID, c)
      appendConfidence explanation = (explanation, hypothesisConfidence mind explanation)

{-| 'applyConstrainers' applies constraining relations to the given
    hypotheses, recalculating their confidence bounds. -}

applyConstrainers :: forall s r c. Mind s r c -> ObjectIDs -> Mind s r c

applyConstrainers mind objects =
    IDSet.fold applyHypothesisConstrainers mind objects
    where
      applyHypothesisConstrainers :: ObjectID -> Mind s r c -> Mind s r c
      applyHypothesisConstrainers object mind =
          let intrinsicConfidence = getItemFromMap (confidence mind) object :: c
              constrainers = getSetFromMap (constrainersOf mind) object :: ConstrainerIDs
              confidenceBounds = IDSet.fold applyConstrainer (intrinsicConfidence, intrinsicConfidence) constrainers :: (c, c)
           in noteEvent (ME_HypothesisEvent object (HE_ConfidenceConstraint constrainers confidenceBounds)) $
              mind { confBounds = IDMap.insert object confidenceBounds (confBounds mind) }
      applyConstrainer :: ConstrainerID -> (c, c) -> (c, c)
      applyConstrainer constrainer confidenceBounds =
          let (_, _, ConstrainerAction action) = getItemFromMap (constrainers mind) constrainer :: (SubjectIDs, ObjectID, ConstrainerAction c)
           in action (hypothesisOpinion mind) confidenceBounds

{-| 'nextSteps' suggests what, if anything, should now be done with each of
    the given hypotheses. -}

nextSteps :: forall s r c. Mind s r c -> s -> HypothesisIDs -> HypothesisMap (r, Status)

nextSteps mind situation hypotheses =    
    let mutableHypotheses = IDSet.difference hypotheses (mappend (unacceptable mind) (irrefutable mind)) :: HypothesisIDs
        suggestions = IDMap.fromDistinctAscList (map getSuggestion (IDSet.toList mutableHypotheses)) :: HypothesisMap (r, Status)
    in IDMap.filterWithKey beliefChange suggestions
    where
      getSuggestion :: HypothesisID -> (HypothesisID, (r, Status))
      getSuggestion hypothesis =
          let bounds =
                  case IDMap.lookup hypothesis (confOverrides mind) of
                    Just c  -> (c, c)
                    Nothing -> getItemFromMap (confBounds mind) hypothesis
          in (hypothesis, (suggester mind) situation bounds)
      beliefChange :: HypothesisID -> (r, Status) -> Bool
      beliefChange hypothesis (_, status) =
          getItemFromMap (beliefStatus mind) hypothesis /= status

{-| 'findChanges' reports those hypotheses whose confidence or belief status
    is different in different minds. -}

-- Note that in the current use of this, a belief status should never be
-- different between the minds, so we do not compare those.

findChanges :: forall s r c. Metric c => HypothesisIDs -> Mind s r c -> Mind s r c -> HypothesisIDs

findChanges relevantHypotheses mind1 mind2 =
    IDSet.filter hasChanged relevantHypotheses
    where
      hasChanged :: HypothesisID -> Bool
      hasChanged hypothesis =
          case (IDMap.lookup hypothesis (confidence mind1), 
                IDMap.lookup hypothesis (confidence mind2)) of
            (Nothing, _) -> True
            (Just confidence1, Just confidence2) ->
                let confBounds1 = getItemFromMap (confBounds mind1) hypothesis :: (c, c)
                    confBounds2 = getItemFromMap (confBounds mind2) hypothesis :: (c, c)
                in confidence1 /= confidence2 || confBounds1 /= confBounds2

-- This is the old version of findChanges that always seems to be slower.

findChanges' :: forall s r c. Metric c => Mind s r c -> Mind s r c -> HypothesisIDs

findChanges' mind1 mind2 =
    let mind1confidence = IDMap.toAscList (confidence mind1) :: [(HypothesisID, c)]
        mind1confBounds = IDMap.toAscList (confBounds mind1) :: [(HypothesisID, (c, c))]
        mind2confidence = IDMap.toAscList (confidence mind2) :: [(HypothesisID, c)]
        mind2confBounds = IDMap.toAscList (confBounds mind2) :: [(HypothesisID, (c, c))]
    in IDSet.fromDistinctAscList (merge (zip mind1confidence mind1confBounds) (zip mind2confidence mind2confBounds))
    where
      merge :: [((HypothesisID, c), (HypothesisID, (c, c)))] -> [((HypothesisID, c), (HypothesisID, (c, c)))] -> [HypothesisID]
      merge [] newConfidences = map (fst . fst) newConfidences
      merge oldConfidences@(((hypothesis1, confidence1), (_, confBounds1)) : remainder1)
                           (((hypothesis2, confidence2), (_, confBounds2)) : remainder2) =
          case compare hypothesis1 hypothesis2 of
            GT -> hypothesis2 : merge oldConfidences remainder2
            EQ -> (if confidence1 == confidence2 && confBounds1 == confBounds2
                   then id else (hypothesis1 :)) $ merge remainder1 remainder2

{-| 'changeBeliefStatus' changes a mutable hypothesis' belief status in a
    mind. It is used by 'reason'. -}

-- Because this function may be called ahead of a reasoning cycle where
-- mustPropagate == False it must cause the propagation of the effects by
-- marking the affected hypotheses as requiring evaluation.

changeBeliefStatus :: Mind s r c -> HypothesisID -> Status -> Mind s r c

changeBeliefStatus mind hypothesis status =
    let removedFrom = 
            case getItemFromMap (beliefStatus mind) hypothesis of
              Accept   -> mind { accepted    = IDSet.delete hypothesis (accepted mind) }
              Consider -> mind { considering = IDSet.delete hypothesis (considering mind) }
              Refute   -> mind { refuted     = IDSet.delete hypothesis (refuted mind) }
        mind' =
            case status of
              Accept   -> removedFrom { accepted    = IDSet.insert hypothesis (accepted removedFrom) }
              Consider -> removedFrom { considering = IDSet.insert hypothesis (considering removedFrom) }
              Refute   -> removedFrom { refuted     = IDSet.insert hypothesis (refuted removedFrom) }
        changeEffects = IDSet.insert hypothesis (getSetFromMap (effects mind) hypothesis) :: HypothesisIDs
    in noteEvents [ME_HypothesisEvent hypothesis (HE_BeliefStatusSet status),
                   ME_HypothesesEvent changeEffects HE_NeedsEvaluation] $
       mind' { beliefStatus = IDMap.insert hypothesis status (beliefStatus mind'),
               unevaluated = mappend changeEffects (unevaluated mind') }

{-| 'reason' examines abductive differentials among the hypotheses in a mind
    and takes possibly many steps of propagating confidence and adjusting
    hypotheses' belief status to incrementally reduce the risk of erroneous
    beliefs. The "Reasoner" module does no inference except for within
    'reason'. -}

reason :: forall s r c.
          (Ord r, Show r, Metric c)
       => ReasonerSettings {- ^ Configure reasoning behavior -}
       -> s            {- ^ The current situation; this value is opaque to the mind -}
       -> Mind s r c   {- ^ The mind in which reasoning should now occur -}
       -> Mind s r c   {- ^ The mind after reasoning is complete -}

reason settings@(ReasonerSettings recalculate) situation mind =
    -- for the first cycle we must guarantee that hypothesis changes that
    --  occurred in between calls to 'reason' will be recognized as changes;
    --  'findChanges' will not notice them, so mustPropagate == True
    reason' recalculate True mind
    where
      reason' :: Bool -> Bool -> Mind s r c -> Mind s r c
      reason' recalculate mustPropagate mind0 =
          let mind1 = (noteEvent (ME_HypothesesEvent relevantHypotheses HE_StartedEvaluation) .
                       noteEvent (ME_StartedReasoning settings)) mind0
              -- note the current state of belief
              believed = mappend (irrefutable mind1) (accepted mind1) :: HypothesisIDs
              notDisbelieved = mappend believed (considering mind1) :: HypothesisIDs
              allHypotheses = mconcat [refuted mind1, unacceptable mind1, notDisbelieved] :: HypothesisIDs
              -- note the relevant hypotheses
              relevantHypotheses = if recalculate then allHypotheses else unevaluated mind1 :: HypothesisIDs
              -- we must evaluate all the differentials they participate in
              relevantDifferentials = findActiveDifferentials mind1 relevantHypotheses :: [(ObjectID, SubjectIDs)]
              -- find all the adjusting relations that affect any of the hypotheses
              affectingAdjusterIDs = mconcat (map (getSetFromMap (adjustersOf mind1)) (IDSet.toList relevantHypotheses)) :: AdjusterIDs
              -- organize them into dependency order
              filteredDependencies = (if recalculate then id else map (IDSet.intersection affectingAdjusterIDs))
                                     (independent mind1 : dependencies mind1) :: [AdjusterIDs]
              -- then reset the hypotheses to a priori confidence
              mind2 = resetConfidences mind1 situation relevantHypotheses
              -- apply the adjusting relations
              mind3 = applyAdjusters mind2 filteredDependencies
              -- apply the confidence boosts
              mind4 = foldl' (uncurry . processDifferential relevantHypotheses) mind3 relevantDifferentials
              -- apply the logical constrainers
              mind5 = applyConstrainers mind4 relevantHypotheses
              -- find out which hypotheses' confidence actually changed;
              --  if this is the first round of this reasoning session, assume
              --  that changes include ones set manually by the user since we last
              --  reasoned
              changes = (if mustPropagate then mappend (unevaluated mind1) else id)
                        (IDSet.difference (findChanges relevantHypotheses mind1 mind5) (IDMap.keysSet (confOverrides mind5)))
              -- for those that changed, revise our notes of suggestions
              revisionCandidates = IDSet.difference changes (mappend (irrefutable mind1) (unacceptable mind1)) :: HypothesisIDs
              oldSuggestions = IDSet.fold IDMap.delete (suggestions mind5) revisionCandidates :: HypothesisMap (r, Status)
              newSuggestions = nextSteps mind5 situation revisionCandidates :: HypothesisMap (r, Status)
              allSuggestions = if recalculate then nextSteps mind5 situation (mconcat [accepted mind1, considering mind1, refuted mind1])
                                              else IDMap.union newSuggestions oldSuggestions :: HypothesisMap (r, Status)
              -- figure out which hypotheses next need to be evaluated, those where
              --  a change in this cycle affects a hypotheses we had not already
              --  evaluated in this cycle
              reevaluate = IDSet.difference (mconcat (map (getSetFromMap (effects mind5)) (IDSet.toList changes))) relevantHypotheses :: HypothesisIDs
              mind6 = noteEvent (ME_HypothesesEvent relevantHypotheses HE_NeedsEvaluation) $
                      mind5 { unevaluated = if recalculate then mempty else reevaluate,
                              suggestions = allSuggestions }
              -- see what the next best (least-risky) belief state change is, if any
              maybeCourseOfAction = listToMaybe (sortBy (on compare snd) (IDMap.toList (suggestions mind6))) :: Maybe (HypothesisID, (r, Status))
              Just (hypothesisToChange, (_, newStatus)) = maybeCourseOfAction
              -- make that change
              mind7 = changeBeliefStatus mind6 hypothesisToChange newStatus
              mind8 = mind7 { suggestions = IDMap.delete hypothesisToChange (suggestions mind7) }
              -- helper functions for if we reason again or not
              wrapUp :: Bool -> Mind s r c -> Mind s r c
              wrapUp again = noteEvents [ME_HypothesesEvent relevantHypotheses HE_FinishedEvaluation, 
                                         if again then ME_ReinvokingReasoner else ME_FinishedReasoning]
              -- now a cycle is done we can let recalculate == False and mustPropagate == False
              reasonAgain = reason' False False . wrapUp True :: Mind s r c -> Mind s r c
              -- so, this is where we figure out what to do
              chooseAction :: Mind s r c
              chooseAction
                  -- there was never any work to do
                  | IDSet.null relevantHypotheses && IDMap.null (suggestions mind0) = wrapUp False mind1
                  -- we must propagate some effects another step
                  | not (IDSet.null (unevaluated mind6)) = reasonAgain mind6
                  -- we have a belief to change
                  | maybeCourseOfAction /= Nothing = reasonAgain mind8
                  -- we have no remaining work
                  | otherwise = wrapUp False mind6
          in chooseAction
