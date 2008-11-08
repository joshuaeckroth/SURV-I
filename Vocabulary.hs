{-| The "Vocabulary" module is a veneer that makes it easy to get going with
    the "Reasoner" module. It is Copyright 2004, 2007, 2008 by Aetion
    Technologies LLC and is proprietary and company confidential. -}

module Vocabulary
where
import Reasoner (Metric (..), Status (..))
import Data.List (sort)

-- Levels

data Level = Lowest | VeryLow | Low | SlightlyLow | Medium | SlightlyHigh | High | VeryHigh | Highest deriving (Bounded, Enum, Eq, Ord, Show)

increaseLevel :: Level -> Level

increaseLevel level =
    if level == maxBound then level else succ level

decreaseLevel :: Level -> Level

decreaseLevel level =
    if level == minBound then level else pred level

increaseLevelBy :: Int -> Level -> Level

increaseLevelBy n level =
    iterate increaseLevel level !! n

decreaseLevelBy :: Int -> Level -> Level

decreaseLevelBy n level =
    iterate decreaseLevel level !! n

instance Metric Level where
    negateMetric level = decreaseLevelBy (fromEnum level) maxBound

type DegreeOfRisk = Level
type DegreeOfConfidence = Level

-- Hypotheses' merit

confidenceBoost :: DegreeOfConfidence -> [DegreeOfConfidence] -> DegreeOfConfidence

confidenceBoost superior [] =
    confidenceBoost superior [minBound]

confidenceBoost superior (inferior : _) =
    let difference = decreaseLevelBy (fromEnum inferior) superior
     in increaseLevelBy (fromEnum difference) superior

suggestStatus :: DegreeOfRisk -> (DegreeOfConfidence, DegreeOfConfidence) -> (DegreeOfRisk, Status)

suggestStatus inconclusionAcceptability (myAcceptanceConfidence, myRejectionConfidence) =
    head (sort [(negateMetric myAcceptanceConfidence,    Accept),
		(negateMetric inconclusionAcceptability, Consider),
		(             myRejectionConfidence,     Refute)])
