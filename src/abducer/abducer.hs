import IO
import System
import System.IO
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent
import Abducer
import Types
import World

main =
    do
      let ws = newWorldState
      debug <- openFile "abducer.log" WriteMode
      outputXmlHeader ws
      frame <- getFrame debug
      processFrame frame ws debug
      outputXmlFooter ws

processFrame :: Either String Frame
             -> WorldState
             -> Handle
             -> IO ()
processFrame (Left s)      ws debug = putStrLn "<!-- Done -->"
processFrame (Right frame) ws debug = do let world    = runAbducer frame ws
                                             log      = outputLog world
                                             (ws', _) = worldState world

                                         putStr log
                                         frame <- getFrame debug
                                         processFrame frame ws' debug

getFrame :: Handle -> IO (Either String Frame)
getFrame debug = do contents <- getLine
                    hPutStrLn debug ("<!-- " ++ contents ++ " -->")
                    return (fromXml $ xmlParse "stream" contents)