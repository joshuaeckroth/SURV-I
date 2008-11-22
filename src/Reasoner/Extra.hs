{-# LANGUAGE ScopedTypeVariables #-} -- only needed to add foralls to permit some optional type signatures

{-| The "Reasoner.Extra" module wraps "Reasoner.Core" to provide convenience
    functions that provide useful constrainers. Only the differences from
    "Reasoner.Core" are documented. It is Copyright 2007, 2008 by Aetion
    Technologies LLC and is proprietary and company confidential. -}

-- TODO: Document the whole module, not just the changes from Reasoner.Core.

module Reasoner.Extra
 ({-* Data types -}
  Core.TransitiveExplains(..), Mind, Core.HypEvent(..), MindEvent(..), ExhaustiveID, ExhaustiveIDs, ExhaustiveMap,
  {-* Querying a 'Mind' -}
  showMind, showHypothesis, hypothesisStatus, unexplained, mindTrace, hypothesisTrace, setTrace,
  unacceptableHypotheses, refutedHypotheses, consideringHypotheses, acceptedHypotheses, irrefutableHypotheses,
  getAdjusters, getConstrainers, getExplainers, getExhaustives, getComposites,
  {-* Creating and manipulating a 'Mind' -}
  newMind, addElementaryHypothesis, addCompositeHypothesis, removeHypothesis, 
  addExhaustive, removeExhaustive, addAdjuster, removeAdjuster, addConstrainer, removeConstrainer, addExplains, removeExplains,
  setCategory, setConfidence, unsetConfidence, setFactual, setCounterfactual, unsetStatus, calculateDependencies,
  {-* Reasoning -}
  Core.ReasonerSettings(..), reason)

where
import Data.Foldable as Foldable (toList)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import WrappedInts.IDMap (getItemFromMap, getSetFromMap)
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import qualified Data.Sequence as Seq
import qualified Reasoner.Core as Core
import WrappedInts.Types
import Reasoner.Types
import Reasoner.Constrainers

data Mind s r c = Mind
    {
      coreMind     :: Core.Mind s r c,
      exhaustives  :: ExhaustiveMap ConstrainerIDs,
      composites   :: HypothesisMap ConstrainerIDs,
      constrainers :: [ConstrainerID],
      history      :: Maybe (Seq.Seq (MindEvent c)) -- Nothing if and only if (Core.history coreMind == Nothing)
    }

-- A phantom type, following the pattern of those in "Reasoner.Types"

data Exhaustive = Exhaustive

{-| A 'ExhaustiveID' is a unique handle for identifying a set of hypotheses of
    which exactly one is true; it exists to type-safely wrap an Int. -}

type ExhaustiveID = HasInt Exhaustive

{-| 'ExhaustiveIDs' is a set of ExhaustiveID's, type-safely wrapped. They are
    used where bulk operations on exhaustive sets may be done considerably
    more efficiently than they would be one at a time. -}

type ExhaustiveIDs = HasInts Exhaustive

{-| 'ExhaustiveMap' is a type-safe wrapping of a map whose keys are of type
    'ExhaustiveID'. -}

type ExhaustiveMap = HasMap Exhaustive

{-| An 'MindEvent' is used as a component of a sequenced log of the activity
    in a mind. It replaces and extends 'Reasoner.Core.MindEvent'. -}

data MindEvent c =
    ME_HypothesisEvent HypothesisID (Core.HypEvent c)
  | ME_HypothesesEvent HypothesisIDs (Core.HypEvent c)
  | ME_AddElementaryHypothesis HypothesisID
  | ME_RemoveElementaryHypothesis HypothesisID
  | ME_AddCompositeHypothesis HypothesisID ConstrainerIDs HypothesisIDs
  | ME_RemoveCompositeHypothesis HypothesisID
  | ME_AddAdjuster AdjusterID SubjectID ObjectID
  | ME_RemoveAdjuster AdjusterID
  | ME_AddExplains ExplainsID SubjectID ObjectID
  | ME_RemoveExplains ExplainsID
  | ME_AddExhaustive ExhaustiveID ConstrainerIDs HypothesisIDs
  | ME_RemoveExhaustive ExhaustiveID
  | ME_StartedReasoning Core.ReasonerSettings
  | ME_ReinvokingReasoner
  | ME_FinishedReasoning
    deriving (Eq, Show)

{-| 'liftMindEvent' lifts 'MindEvent's from "Reasoner.Core" up into
    "Reasoner.Extra". -}

liftMindEvent :: Core.MindEvent c -> Maybe (MindEvent c)

liftMindEvent (Core.ME_HypothesisEvent h he) = Just (ME_HypothesisEvent h he)
liftMindEvent (Core.ME_HypothesesEvent hs he) = Just (ME_HypothesesEvent hs he)
liftMindEvent (Core.ME_AddHypothesis _) = Nothing
liftMindEvent (Core.ME_RemoveHypothesis _) = Nothing
liftMindEvent (Core.ME_AddAdjuster a s o) = Just (ME_AddAdjuster a s o)
liftMindEvent (Core.ME_RemoveAdjuster a) = Just (ME_RemoveAdjuster a)
liftMindEvent (Core.ME_AddExplains e s o) = Just (ME_AddExplains e s o)
liftMindEvent (Core.ME_RemoveExplains e) = Just (ME_RemoveExplains e)
liftMindEvent (Core.ME_StartedReasoning rs) = Just (ME_StartedReasoning rs)
liftMindEvent Core.ME_ReinvokingReasoner = Just ME_ReinvokingReasoner
liftMindEvent Core.ME_FinishedReasoning = Just ME_FinishedReasoning

{- 'updateHistory' brings the 'Reasoner.Extra.Mind''s history up to date with
   that of its wrapped 'Reasoner.Core.Mind'. -}

updateHistory :: forall s r c. Mind s r c -> Mind s r c

updateHistory mind =
    case history mind of
      Nothing -> mind
      Just oldEvents ->
          let newEvents = Seq.fromList (mapMaybe liftMindEvent (Foldable.toList (Core.mindTrace (coreMind mind)))) :: Seq.Seq (MindEvent c)
          in mind { coreMind = Core.setTrace True (coreMind mind),
                    history = Just (oldEvents Seq.>< newEvents) }

{-| 'noteEvent' appends an event to a mind's history log. It must prepend it
    with any other events that were logged in the wrapped mind's history since
    we last checked. -}

noteEvent :: MindEvent c -> Mind s r c -> Mind s r c

noteEvent event mind =
    case history mind of
         Nothing -> mind
         Just _ ->
             let mind' = updateHistory mind
                 Just oldEvents = history mind'
             in mind' { history = Just (oldEvents Seq.|> event) }

{-| 'mindTrace' returns a chronologically-ordered trace of the reasoning
    events in a mind's history. -}

mindTrace :: Mind s r c {- ^ The mind whose history is of interest -}
          -> Seq.Seq (MindEvent c) {- ^ The trace of the reasoning events
                                        since the mind was created -}

mindTrace mind =
    case history (updateHistory mind) of
      Nothing -> Seq.empty
      Just history -> history

{-| 'hypothesisTrace' returns a chronologically-ordered trace of the reasoning
    events in a hypothesis's history. It is selected from what 'mindTrace'
    provides. -}

hypothesisTrace :: HypothesisID {- ^ The hypothesis whose history is of interest -}
                -> Mind s r c {- ^ The mind that holds the hypothesis that is being queried -}
                -> Seq.Seq (Core.HypEvent c) {- ^ The trace of the reasoning events
                                                  since the hypothesis was inserted
                                                  into the mind -}

hypothesisTrace hypothesis mind = 
    Seq.fromList (mapMaybe considerEvent (Foldable.toList (mindTrace mind)))
    where
      considerEvent :: MindEvent c -> Maybe (Core.HypEvent c)
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
    mind { coreMind = Core.setTrace trace (coreMind mind),
           history = if trace then Just Seq.empty else Nothing }

{-| 'getExhaustives' returns all exhaustive choice sets that are in a mind. -}

getExhaustives :: Mind s r c {- ^ The mind whose exhaustive choice sets are being queried -}
               -> [(ExhaustiveID, HypothesisIDs)] {- ^ All exhaustive choice sets -}

getExhaustives mind =
    map (uncurry getExhaustive) (IDMap.toList (exhaustives mind))
    where
      objectLookup :: ConstrainerID -> ObjectID
      objectLookup = getItemFromMap (IDMap.fromList ([ (c, o) | (c, _, o) <- getConstrainers mind ]))
      getExhaustive :: ExhaustiveID -> ConstrainerIDs -> (ExhaustiveID, HypothesisIDs)
      getExhaustive exhaustive constrainerIDs =
          (exhaustive, IDSet.fromList (map objectLookup (IDSet.toList constrainerIDs)))

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
        -> Core.TransitiveExplains {- ^ If this mind should follow explanatory
                                        relations transitively in finding
                                        abductive differentials and, if so, if
                                        it should cache results -}
        -> [ConstrainerID] {- ^ A source of 'ConstrainerID's that do not
                                overlap with any that are to be supplied
                                through 'addConstrainer' -}
        -> Mind s r c {- ^ The new, empty mind, configured as specified -}

newMind boosterFunction suggesterFunction transitiveExplains unusedConstrainers =
    Mind (Core.newMind boosterFunction suggesterFunction transitiveExplains)
         IDMap.empty IDMap.empty unusedConstrainers Nothing

{-| 'getComposites' returns all part-of relationships between hypotheses that
    are in a mind. -}

getComposites :: Mind s r c {- ^ The mind whose composite hypotheses are being queried -}
              -> [(HypothesisID, HypothesisIDs)] {- ^ All composite hypotheses and their elements -}

getComposites mind =
    map (uncurry getComposite) (IDMap.toList (composites mind))
    where
      constrainerLookup :: ConstrainerID -> (SubjectIDs, ObjectID)
      constrainerLookup = getItemFromMap (IDMap.fromList ([ (c, (s, o)) | (c, s, o) <- getConstrainers mind ]))
      getComposite :: HypothesisID -> ConstrainerIDs -> (HypothesisID, HypothesisIDs)
      getComposite composite constrainerIDs =
          let [elements] = [ subjects | (subjects, object) <- map constrainerLookup (IDSet.toList constrainerIDs),
                                        object == composite ]
          in (composite, elements)

-- It is possible that the patterns seen in functions to do with the composite
-- hypotheses and exhaustive sets could be abstracted out and generalized into
-- a constrainer management system that understands one-or-many - to -
-- one-or-many types of constraint.

{-| 'addCompositeHypothesis' adds a given composite hypothesis to a mind,
    putting it in the given category and in the correct relation with its
    parts which may themselves be elementary or composite. It uses 'implies'
    and 'impliedBy'. -}

addCompositeHypothesis :: (Metric c, Ord r) 
                       => HypothesisID  {- ^ A unique ID number for identifying the hypothesis -}
                       -> HypothesisIDs {- ^ The ID numbers of the composite hypothesis' parts -}
                       -> CategoryID    {- ^ The ID number of the category that the hypothesis is in -}
                       -> (s -> c)      {- ^ A function for calculating the hypothesis'
                                             a priori confidence from the current situation -}
                       -> Mind s r c    {- ^ The mind to which the hypothesis is to be added -}
                       -> Mind s r c    {- ^ The mind with the given hypothesis added -}

addCompositeHypothesis composite elements category hypAPriori mind =
    let setSize = IDSet.size elements + 1 :: Int
        (constrainerList@(impliedByConstrainer : impliesConstrainers), unusedConstrainers) =
            splitAt setSize (constrainers mind) :: ([ConstrainerID], [ConstrainerID])
        constrainerSet = IDSet.fromList constrainerList :: ConstrainerIDs
        newConstrainers = (impliedByConstrainer, elements, composite, impliedBy) :
                          [ (constrainer, IDSet.singleton composite, element, implies)
                            | (constrainer, element) <- zip impliesConstrainers (IDSet.toList elements) :: [(ConstrainerID, ObjectID)] ] 
                        :: Metric c => [(ConstrainerID, SubjectIDs, ObjectID, [c] -> [Bound c])]
    in noteEvent (ME_AddCompositeHypothesis composite constrainerSet elements) $
       (flip (foldl' addConstrainer')) newConstrainers $
       addElementaryHypothesis composite category hypAPriori $
       mind { composites = IDMap.insertNew composite constrainerSet (composites mind),
              constrainers = unusedConstrainers }
    where
      addConstrainer' :: Metric c => Mind s r c -> (ConstrainerID, SubjectIDs, ObjectID, [c] -> [Bound c]) -> Mind s r c
      addConstrainer' mind (constrainer, subjects, object, constraints) = 
          addConstrainer constrainer subjects constraints object mind

{-| 'removeHypothesis' removes a hypothesis from a mind. It is not an error if
    the hypothesis did not exist in the mind. Removing a part of a composite
    hypothesis causes that composite hypothesis to also be removed. -}

removeHypothesis :: HypothesisID {- ^ The ID number of the hypothesis that is to be removed -}
                 -> Mind s r c   {- ^ The mind from which the hypothesis is to be removed -}
                 -> Mind s r c   {- ^ The mind with the given hypothesis and the hypothesis' relations removed -}

removeHypothesis hypothesis mind1 =
    let constrainersToRemove = [ constrainer | (constrainer, subjects, object) <- getConstrainers mind1,
                                               IDSet.member hypothesis subjects || object == hypothesis ] :: [ConstrainerID]
        exhaustivesToRemove  = [ exhaustive  | (exhaustive, constrainers) <- IDMap.toList (exhaustives mind1),
                                               or (map (`IDSet.member` constrainers) constrainersToRemove) ] :: [ExhaustiveID]
        compositesToRemove   = [ composite   | (composite, constrainers) <- IDMap.toList (IDMap.delete hypothesis (composites mind1)),
                                               or (map (`IDSet.member` constrainers) constrainersToRemove) ] :: [HypothesisID]
        mind2 = foldl' (flip removeExhaustive) mind1 exhaustivesToRemove
        mind3 = foldl' (flip removeHypothesis) mind2 compositesToRemove
        mind4 = mind3 { coreMind = Core.removeHypothesis hypothesis (coreMind mind3),
                        constrainers = constrainersToRemove ++ constrainers mind3 }
    in case IDMap.lookup hypothesis (composites mind1) of
         Nothing ->           noteEvent (ME_RemoveElementaryHypothesis hypothesis) mind4
         Just constrainers -> noteEvent (ME_RemoveCompositeHypothesis hypothesis) $
                              mind4 { composites = IDMap.delete hypothesis (composites mind4) }

{-| 'addExhaustive' sets a constraint in a mind that exactly one of the given
    hypotheses is true; 'reason' will take this into account in attempting to
    set the confidence and belief status of hypotheses, but it may violate the
    constraint. It an error if the hypotheses to which it refers have not
    already been added to the mind with 'addElementaryHypothesis' or
    'addCompositeHypothesis'. It uses 'oneOf'. -}

-- For each of the hypotheses in the set we add a constraining relation that
-- has it as the object of the others' constraining.

addExhaustive :: Metric c
              => ExhaustiveID  {- ^ A unique ID number for identifying the exhaustive choice set -}
              -> HypothesisIDs {- ^ The ID numbers of the hypotheses of which the choice set is composed -}
              -> Mind s r c    {- ^ The mind to which the exhaustive choice set is to be added -}
              -> Mind s r c    {- ^ The mind with the given exhaustive choice set added -}

addExhaustive exhaustive hypotheses mind =
    let setSize = IDSet.size hypotheses :: Int
        (constrainerList, unusedConstrainers) = splitAt setSize (constrainers mind) :: ([ConstrainerID], [ConstrainerID])
        constrainerIDs = IDSet.fromList constrainerList :: ConstrainerIDs
        newConstrainers = [ (constrainer, IDSet.delete object hypotheses, object) 
                            | (constrainer, object) <- zip constrainerList (IDSet.toList hypotheses) :: [(ConstrainerID, ObjectID)] ]
                        :: [(ConstrainerID, SubjectIDs, ObjectID)]
        mind' = foldl' addConstrainer' mind newConstrainers
     in noteEvent (ME_AddExhaustive exhaustive constrainerIDs hypotheses) $
        mind' { exhaustives = IDMap.insertNew exhaustive constrainerIDs (exhaustives mind'),
                constrainers = unusedConstrainers }
    where
      addConstrainer' :: Metric c => Mind s r c -> (ConstrainerID, SubjectIDs, ObjectID) -> Mind s r c
      addConstrainer' mind (constrainer, subjects, object) = 
          addConstrainer constrainer subjects oneOf object mind

{-| 'removeExhaustive' removes a constraint from a mind that was set by
    'addExhaustive'. It is not an error if the constraint did not exist in the
    mind. -}

removeExhaustive :: ExhaustiveID {- ^ The ID number of the exhaustive choice set that is to be removed -}
                 -> Mind s r c   {- ^ The mind from which the exhaustive choice set is to be removed -}
                 -> Mind s r c   {- ^ The mind with the given exhaustive choice set removed -}

removeExhaustive exhaustive mind =
    if IDMap.notMember exhaustive (exhaustives mind) then mind
    else let constrainersToRemove = getSetFromMap (exhaustives mind) exhaustive :: ConstrainerIDs
             mind' = IDSet.fold removeConstrainer mind constrainersToRemove
     in noteEvent (ME_RemoveExhaustive exhaustive) $
        mind' { exhaustives = IDMap.delete exhaustive (exhaustives mind'),
                constrainers = IDSet.toList constrainersToRemove ++ constrainers mind' }

-- Wrapping of unchanged functions

showMind :: Mind s r c -> [String]
showHypothesis :: Metric c => HypothesisID -> Mind s r c -> [String]
hypothesisStatus :: HypothesisID -> Mind s r c -> (CategoryID, Maybe (c, c), Maybe c, Status)
unexplained :: Mind s r c -> [(CategoryID, HypothesisID)]
unacceptableHypotheses :: Mind s r c -> HypothesisIDs
refutedHypotheses :: Mind s r c -> HypothesisIDs
consideringHypotheses :: Mind s r c -> HypothesisIDs
acceptedHypotheses :: Mind s r c -> HypothesisIDs
irrefutableHypotheses :: Mind s r c -> HypothesisIDs
getAdjusters :: Mind s r c -> [(AdjusterID, SubjectID, ObjectID)]
getConstrainers :: Mind s r c -> [(ConstrainerID, SubjectIDs, ObjectID)]
getExplainers :: Mind s r c -> [(ExplainsID, SubjectID, ObjectID)]
addElementaryHypothesis :: (Metric c, Ord r) => HypothesisID -> CategoryID -> (s -> c) -> Mind s r c -> Mind s r c
addAdjuster :: AdjusterID -> SubjectID -> Either (Maybe (c -> c), Maybe (c -> c)) (Maybe (c -> c -> c), Maybe (c -> c -> c)) -> ObjectID -> Mind s r c -> Mind s r c
removeAdjuster :: AdjusterID -> Mind s r c -> Mind s r c
addConstrainer :: Metric c => ConstrainerID -> SubjectIDs -> ([c] -> [Bound c]) -> ObjectID -> Mind s r c -> Mind s r c
removeConstrainer :: ConstrainerID -> Mind s r c -> Mind s r c
addExplains :: ExplainsID -> SubjectID -> ObjectID -> Mind s r c -> Mind s r c
removeExplains :: ExplainsID -> Mind s r c -> Mind s r c
setCategory :: (Metric c, Ord r) => HypothesisID -> CategoryID -> Mind s r c -> Mind s r c
setConfidence :: (Metric c, Ord r) => HypothesisIDs -> c -> Mind s r c -> Mind s r c
unsetConfidence :: (Metric c, Ord r) => HypothesisIDs -> Mind s r c -> Mind s r c
setFactual :: (Metric c, Ord r) => HypothesisIDs -> Mind s r c -> Mind s r c
setCounterfactual :: (Metric c, Ord r) => HypothesisIDs -> Mind s r c -> Mind s r c
unsetStatus :: (Metric c, Ord r) => HypothesisIDs -> Mind s r c -> Mind s r c
calculateDependencies :: Mind s r c -> Mind s r c
reason :: (Ord r, Show r, Metric c) => Core.ReasonerSettings -> s -> Mind s r c -> Mind s r c

showMind mind = Core.showMind (coreMind mind)
showHypothesis hypothesis mind = Core.showHypothesis hypothesis (coreMind mind)
hypothesisStatus hypothesis mind = Core.hypothesisStatus hypothesis (coreMind mind)
unexplained mind = Core.unexplained (coreMind mind)
unacceptableHypotheses mind = Core.unacceptableHypotheses (coreMind mind)
refutedHypotheses mind = Core.refutedHypotheses (coreMind mind)
consideringHypotheses mind = Core.consideringHypotheses (coreMind mind)
acceptedHypotheses mind = Core.acceptedHypotheses (coreMind mind)
irrefutableHypotheses mind = Core.irrefutableHypotheses (coreMind mind)
getAdjusters mind = Core.getAdjusters (coreMind mind)
getConstrainers mind = Core.getConstrainers (coreMind mind)
getExplainers mind = Core.getExplainers (coreMind mind)
addElementaryHypothesis hypothesis category hypAPriori mind = mind { coreMind = Core.addHypothesis hypothesis category hypAPriori (coreMind mind) }
addAdjuster adjuster subject functions object mind = mind { coreMind = Core.addAdjuster adjuster subject functions object (coreMind mind) }
removeAdjuster adjuster mind = mind { coreMind = Core.removeAdjuster adjuster (coreMind mind) }
addConstrainer constrainer subjects function object mind = mind { coreMind = Core.addConstrainer constrainer subjects function object (coreMind mind) }
removeConstrainer constrainer mind = mind { coreMind = Core.removeConstrainer constrainer (coreMind mind) }
addExplains explainer subject object mind = mind { coreMind = Core.addExplains explainer subject object (coreMind mind) }
removeExplains explainer mind = mind { coreMind = Core.removeExplains explainer (coreMind mind) }
setCategory hypothesis category mind = mind { coreMind = Core.setCategory hypothesis category (coreMind mind) }
setConfidence hypotheses confidenceValue mind = mind { coreMind = Core.setConfidence hypotheses confidenceValue (coreMind mind) }
unsetConfidence hypotheses mind = mind { coreMind = Core.unsetConfidence hypotheses (coreMind mind) }
setFactual hypotheses mind = mind { coreMind = Core.setFactual hypotheses (coreMind mind) }
setCounterfactual hypotheses mind = mind { coreMind = Core.setCounterfactual hypotheses (coreMind mind) }
unsetStatus hypotheses mind = mind { coreMind = Core.unsetStatus hypotheses (coreMind mind) }
calculateDependencies mind = mind { coreMind = Core.calculateDependencies (coreMind mind) }
reason settings situation mind = mind { coreMind = Core.reason settings situation (coreMind mind) }
