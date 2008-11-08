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
import WrappedInts.IDSet (fromList)
import WrappedInts.Types (HasInt(..))

main =
    do
        args <- System.getArgs
        frames <- fReadXml $ head args :: IO Frames
        putStrLn $ processAll frames

processAll :: Frames -> String
processAll (Frames frames) =
    let
        mind = newMind confidenceBoost suggestStatus SparseTransitive (map HasInt [1..] :: [ConstrainerID])
        in
          processFrames frames mind

--processFrames :: forall s r c.
--                 (Ord r, Show r, Metric c)
--              => [Frame]
--              -> Mind s r c
--              -> String

processFrames []                          _    = "End of frame\n\n"
processFrames ((Frame attrs acqs):frames) mind =
    let
        hs        = map HasInt [0..] :: [HypothesisID]
        category  = HasInt 0 :: CategoryID
        mind      = processAcquisitions acqs mind hs category
        in 
          "Mind for frame\n" ++ unlines (showMind mind) ++ "\n" ++ processFrames frames mind

--processAcquisitions :: forall s r c.
--                       (Ord r, Show r, Metric c)
--                    => [Acquisition]
--                    -> Mind s r c
--                    -> [HypothesisID]
--                    -> CategoryID
--                    -> Mind s r c

processAcquisitions []     mind _  _     = reason (ReasonerSettings False) High mind
processAcquisitions (a:as) mind hs catID =
    let
        acqID    = read $ acquisitionId a
        newMind  = setFactual (fromList [(hs!!acqID)])
                   (addElementaryHypothesis (hs!!acqID) catID (const Medium) mind)
        in
          processAcquisitions as newMind hs catID
    

