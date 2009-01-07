import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.Combinators
import Abducer
import Types
import World

main =
    do
      let ws = newWorldState
      args <- System.getArgs
      (Frames frames) <- mergeFrameByFrame (args!!0) (args!!1)
      runAbducer frames ws

mergeFrameByFrame :: String -> String -> IO Frames
mergeFrameByFrame input_0 input_1 =
    do
      (Frames frames_camera_0) <- fReadXml input_0
      (Frames frames_camera_1) <- fReadXml input_1
      return $ mergedFrames frames_camera_0 frames_camera_1

    where
      mergedFrames :: [Frame] -> [Frame] -> Frames
      mergedFrames [] _              = Frames []
      mergedFrames _ []              = Frames []
      mergedFrames (f0:f0s) (f1:f1s) =
          let
              (Frame attrs _)  = f0
              (time, num)      = (frameTime attrs, frameNumber attrs)
              (Frame _ acqs_0) = f0
              (Frame _ acqs_1) = f1
              (Frames rest)    = mergedFrames f0s f1s
          in
          Frames ([Frame
                   (Frame_Attrs time num)
                   (acqs_0 ++ acqs_1)]
                  ++ rest)