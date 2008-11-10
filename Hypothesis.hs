module Hypothesis where
import Acquisition
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import qualified Data.IntSet as IntSet (findMax)
import qualified WrappedInts.IDMap as IDMap
import WrappedInts.Types

generateNoiseHypotheses :: -- forall s r c.
                           (Ord r, Show r, Metric c)
                        => [AcquisitionID] 
                        -> [HypothesisID]
                        -> CategoryID
                        -> Mind s r c
                        -> ([HypothesisID], Mind s r c)

generateNoiseHypotheses []     hs _     mind = (hs, mind)
generateNoiseHypotheses (a:as) hs catID mind =
    generateNoiseHypotheses as newHs catID newMind
        where
          hypID      = 1 + (head hs)
          newHs      = [hypID] ++ hs
          explainID  = HasInt (nextID explainers mind)
          newMind    = addExplains explainID hypID a
                       (addHypothesis hypID catID (scoreNoiseHypothesis hypID) mind)

-- scoreNoiseHypothesis :: HypothesisID -> s -> c
scoreNoiseHypothesis hypID _ = High

nextID field mind = 1 + (IntSet.findMax $ wrappedInts $ IDMap.keysSet $ field mind)

