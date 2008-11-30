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
import WrappedInts.IDSet (toList)

-- | Execute the abduction
runAbducer :: [Frame]    -- ^ List of frames (which contain acquisitions)
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
                catID    = HasInt 0 :: CategoryID
                world    = ((return $ cleanWorld f ws) >>=
                            recordFrame >>=

                            hypothesizeAcquisitions catID >>=
                            hypothesizeNoise catID >>=
                            hypothesizeTracks catID >>=
                            hypothesizeSplitTracks catID >>=
                            -- hypothesizeClassifications catID >>=
                            constrainAcquisitionExplainers >>=

                            (\ws'' -> return ws'' { mind = (reason (ReasonerSettings False) Medium (mind ws'')) }) >>=

                            updateNoise >>=
                            updateTracks >>=
--                            updateAcquisitions >>=

                            recordNoise >>=
                            recordTracks >>=
                            recordAcquisitions)

                (ws', _) = worldState world

            outputLog world
            runAbducer' fs ws'
