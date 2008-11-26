module Abducer where
import Types
import World
import Frame
import Acquisition
import Noise
import Track
import Vocabulary
import Reasoner.Types
import Reasoner.Core
import WrappedInts.Types (HasInt(..))

-- | Execute the abduction
runAbducer :: [Frame]          -- ^ List of frames (which contain acquisitions)
           -> WorldState -- ^ Existing world
           -> IO ()
runAbducer frames ws =
    do
      outputXmlHeader ws
      runAbducer' frames ws
      outputXmlFooter ws

    where
      runAbducer' :: [Frame] -> WorldState -> IO ()
      runAbducer' []     _  = return ()
      runAbducer' (f:fs) ws =
          do
            let
                catID = HasInt 0 :: CategoryID
                -- insert frame and reset 'current' acquisition, noise, track hypotheses,
                -- then execute abduction sequence
                world    = ((return $ ws { frame = f, acqIDs = [], noiseIDs = [], trackIDs = [] }) >>=
                            recordFrame >>=
                            hypothesizeAcquisitions catID >>=
                            hypothesizeNoise catID >>=
                            hypothesizeTracks catID >>=
                            -- hypothesizeClassifications catID >>=
                            constrainAcquisitionExplainers >>=
                            (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) Medium (mind ws'')) }) >>=
                            recordAcquisitions >>=
                            updateNoise >>=
                            recordNoise >>=
                            updateTracks >>=
                            recordTracks)
                (ws', _) = worldState world

            outputLog world
            runAbducer' fs ws'
