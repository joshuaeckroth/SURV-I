import IO
import System
import System.IO
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent
import Abducer
import Types
import World

main = do
  let ws = newWorldState
  input <- openFile "detections.xml" ReadMode
  frame <- getFrame input
  processFrame input frame ws

processFrame :: Handle
             -> Either String Frame
             -> WorldState
             -> IO ()
processFrame _     (Left s)      ws = putStrLn "<!-- Done -->"
processFrame input (Right frame) ws = case frame of
                                        (Frame _ []) -> do
                                                   putStrLn ("Skipping empty frame " ++ (show $ frameProp frameNumber frame))
                                                   frame <- getFrame input
                                                   processFrame input frame ws
                                        otherwise    ->
                                            do let world    = runAbducer frame ws
                                                   log      = (xmlHeader ws) ++ (outputLog world) ++ (xmlFooter ws)
                                                   (ws', _) = worldState world

                                               frameFile <- openFile ("tracks/frame-" ++ (show $ frameProp frameNumber frame) ++ ".xml") WriteMode
                                               hPutStrLn frameFile log
                                               hClose frameFile
                                               putStrLn ("Track file written for frame " ++ (show $ frameProp frameNumber frame))
                                               frame <- getFrame input
                                               processFrame input frame ws'

getFrame :: Handle -> IO (Either String Frame)
getFrame input = do 
  contents <- hGetLine input
  return (fromXml $ xmlParse "stream" contents)