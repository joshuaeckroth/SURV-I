module Abducer where
import IO
import System
import Control.Arrow ((>>>))
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ

import Acquisition
import Hypothesis
import Reasoner
import Vocabulary
import WrappedInts.IDSet (fromList,toList)
import WrappedInts.Types (HasInt(..))


processAll :: Frames -> String
processAll (Frames frames) =
    let
        hs   = [HasInt 0] :: [HypothesisID]
        mind = newMind confidenceBoost suggestStatus SparseTransitive (map HasInt [1..] :: [ConstrainerID])
        in
          processFrames frames hs mind

processFrames :: forall s r c.
                 (Ord r, Show r, Metric c)
              => [Frame]
              -> [HypothesisID]
              -> Mind s r c
              -> String

processFrames []                          _  _    = "End of frame\n\n"
processFrames ((Frame attrs acqs):frames) hs mind =
    let
        catID            = HasInt 0 :: CategoryID
        (newHs, newMind) = processAcquisitions acqs hs catID mind
        in 
          "Mind for frame " ++ show (frameNumber attrs) ++ "\n" ++
                            unlines (showMind newMind) ++ "\n" ++
                            processFrames frames newHs newMind

processAcquisitions :: forall s r c.
                       (Ord r, Show r, Metric c)
                    => [Acquisition]
                    -> [HypothesisID]
                    -> CategoryID
                    -> Mind s r c
                    -> ([HypothesisID], Mind s r c)

processAcquisitions acqs hs catID mind =
    let
        (acqIDs, hs1, mind1) = generateAcquisitionHypotheses acqs hs catID mind
        (hs2, mind2)         = generateNoiseHypotheses acqIDs hs catID mind1
    in
      (hs2, mind2)
