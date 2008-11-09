module Abducer where
import IO
import System
import Control.Arrow ((>>>))
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ

import AcquisitionTypes
import AcquisitionFuncs
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

--processFrames :: forall s r c.
--                 (Ord r, Show r, Metric c)
--              => [Frame]
--              -> Mind s r c
--              -> String

processFrames []                          _  _    = "End of frame\n\n"
processFrames ((Frame attrs acqs):frames) hs mind =
    let
        category         = HasInt 0 :: CategoryID
        (newMind, newHs) = processAcquisitions acqs mind hs category
        in 
          "Mind for frame " ++ show (frameNumber attrs) ++ "\n" ++
                            unlines (showMind newMind) ++ "\n" ++
                            processFrames frames newHs newMind

--processAcquisitions :: forall s r c.
--                       (Ord r, Show r, Metric c)
--                    => [Acquisition]
--                    -> Mind s r c
--                    -> [HypothesisID]
--                    -> CategoryID
--                    -> Mind s r c

processAcquisitions []     mind hs _     = (reason (ReasonerSettings False) High mind, hs)
processAcquisitions (a:as) mind hs catID = processAcquisitions as newMind newHs catID
    where
        acqID    = 1 + (head hs)
        newHs    = [acqID] ++ hs
        newMind  = setFactual (fromList [acqID])
                   (addElementaryHypothesis acqID catID (const Medium) mind)

