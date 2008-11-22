{-| The "Reasoner.Types" module provides the core type definitions for the
    reasoner. It is Copyright 2007, 2008 by Aetion Technologies LLC and is
    proprietary and company confidential. -}

module Reasoner.Types
 (Metric(..), Bound(..), Status(..),
  HypothesisID, HypothesisIDs, HypothesisMap, CategoryID, CategoryIDs, CategoryMap,
  AdjusterID, AdjusterIDs, AdjusterMap, ExplainsID, ExplainsIDs, ExplainsMap,
  ConstrainerID, ConstrainerIDs, ConstrainerMap,
  SubjectID, SubjectIDs, SubjectMap, ObjectID, ObjectIDs, ObjectMap)

where
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
import WrappedInts.Types

{-| The 'Metric' class is for confidence values. We need to be able to negate
    confidence values in implementing some useful constraining relations. -}

class (Eq m, Ord m, Show m) => Metric m where
    {-| Negate a value -}
    negateMetric :: m -> m

{-| A 'Bound' is used for expressing constraints on confidence. Constraining
    relations can cause these to come into force on the hypothesis that is the
    object of the relation. -}

data Metric c => Bound c = 
      {-| A 'HigherThan' bound is applied when we consider accepting a hypothesis. -}
      HigherThan c
      {-| A 'LowerThan' bound is applied when we consider refuting a hypothesis. -}
    | LowerThan  c deriving (Eq, Show)

{-| The 'Status' type is used for representing the belief status of a
    hypothesis. -}

data Status = 
    {-| Disbelieved -}
    Refute 
    {-| Believed -}
  | Accept 
    {-| Undecided -}
  | Consider deriving (Eq, Ord, Show)

-- These few are used as phantom types

data Hypothesis  = Hypothesis
data Category    = Category
data Adjuster    = Adjuster
data Explains    = Explains
data Constrainer = Constrainer
type Subject = Hypothesis
type Object  = Hypothesis

{-| A 'HypothesisID' is a unique handle for identifying a hypothesis; it
    exists to type-safely wrap an Int. -}

type HypothesisID = HasInt Hypothesis

{-| 'HypothesisIDs' is a set of HypothesisID's, type-safely wrapped. They are
    used where bulk operations on hypotheses may be done considerably more
    efficiently than they would be one at a time. -}

type HypothesisIDs = HasInts Hypothesis

{-| 'HypothesisMap' is a type-safe wrapping of a map whose keys are of type
    'HypothesisID'. -}

type HypothesisMap = HasMap Hypothesis

-- TODO: Enforce the below constraint.

{-| A 'CategoryID' is a unique handle for identifying a contrast category; it
    exists to type-safely wrap an Int. Hypotheses have to be in the same
    contrast category in order to be rivals. Hypotheses within a contrast
    category may not explain each other. -}

type CategoryID = HasInt Category

{-| 'CategoryIDs' is a set of CategoryID's, type-safely wrapped. They are used
    where bulk operations on categories may be done considerably more
    efficiently than they would be one at a time. -}

type CategoryIDs = HasInts Category

{-| 'CategoryMap' is a type-safe wrapping of a map whose keys are of type
    'CategoryID'. -}

type CategoryMap = HasMap Category

{-| A 'AdjusterID' is a unique handle for identifying a confidence-adjusting
    relation such as \'supports\'; it exists to type-safely wrap an Int. -}

type AdjusterID = HasInt Adjuster

{-| 'AdjusterIDs' is a set of AdjusterID's, type-safely wrapped. They are used
    where bulk operations on adjusters may be done considerably more
    efficiently than they would be one at a time. -}

type AdjusterIDs = HasInts Adjuster

{-| 'AdjusterMap' is a type-safe wrapping of a map whose keys are of type
    'AdjusterID'. -}

type AdjusterMap = HasMap Adjuster

{-| An 'ExplainsID' is a unique handle for identifying the relation that one
    hypothesis is an explanation for another; it exists to type-safely wrap an
    Int. -}

type ExplainsID = HasInt Explains

{-| 'ExplainsIDs' is a set of ExplainsID's, type-safely wrapped. They are used
    where bulk operations on explains relations may be done considerably more
    efficiently than they would be one at a time. -}

type ExplainsIDs = HasInts Explains

{-| 'ExplainsMap' is a type-safe wrapping of a map whose keys are of type
    'ExplainsID'. -}

type ExplainsMap = HasMap Explains

{-| A 'ConstrainerID' is a unique handle for identifying a constraining
    relation. 'ConstrainerID' exists to type-safely wrap an Int. -}

type ConstrainerID = HasInt Constrainer

{-| 'ConstrainerIDs' is a set of ConstrainerIDs, type-safely wrapped. They are
    used where bulk operations on constraining relations may be done
    considerably more efficiently than they would be one at a time. -}

type ConstrainerIDs = HasInts Constrainer

{-| 'ConstrainerMap' is a type-safe wrapping of a map whose keys are of type
    'ConstrainerID'. -}

type ConstrainerMap = HasMap Constrainer

{-| 'SubjectID' is a synonym for 'HypothesisID' that is used to make it
    clearer in a type signature that a hypothesis is the subject of a
    relation. -}

type SubjectID = HypothesisID

{-| 'SubjectIDs' is a synonym for 'HypothesisIDs' that is used to make it
    clearer in a type signature that the hypotheses are subjects of relations.
    -}

type SubjectIDs = HypothesisIDs

{-| 'SubjectMap' is a synonym for 'HypothesisMap' that is used to make it
    clearer in a type signature that the hypotheses are subjects of relations.
    -}

type SubjectMap = HasMap Subject

{-| 'ObjectID' is a synonym for 'HypothesisID' that is used to make it clearer
    in a type signature that a hypothesis is the object of a relation. -}

type ObjectID = HypothesisID

{-| 'ObjectIDs' is a synonym for 'HypothesisID' that is used to make it
    clearer in a type signature that the hypotheses are objects of relations.
    -}

type ObjectIDs = HypothesisIDs

{-| 'ObjectMap' is a synonym for 'HypothesisID' that is used to make it
    clearer in a type signature that the hypotheses are objects of relations.
    -}

type ObjectMap = HasMap Object
