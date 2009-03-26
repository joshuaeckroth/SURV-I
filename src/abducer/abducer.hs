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
  frame <- getFrame
  processFrame frame ws

processFrame :: Either String Frame
             -> WorldState
             -> IO ()
processFrame (Left s)      ws = putStrLn "<!-- Done -->"
processFrame (Right frame) ws = do let world    = runAbducer frame ws
                                       log      = (xmlHeader ws) ++ (outputLog world) ++ (xmlFooter ws)
                                       (ws', _) = worldState world

                                   frameFile <- openFile ("tracks/frame-" ++ (show $ frameProp frameNumber frame) ++ ".xml") WriteMode
                                   hPutStrLn frameFile log
                                   hClose frameFile
                                   putStrLn "Track file written."
                                   frame <- getFrame
                                   processFrame frame ws'

getFrame :: IO (Either String Frame)
getFrame = do 
  contents <- getLine
  return (fromXml $ xmlParse "stream" contents)