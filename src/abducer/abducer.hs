import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent
import Abducer
import Types
import World

main =
    do
      let ws = newWorldState
      --outputXmlHeader ws
      frame <- getFrame
      processFrame frame ws
      --outputXmlFooter ws

processFrame :: Either String Frame
             -> WorldState
             -> IO ()
processFrame (Left s)      ws = putStrLn "<!-- Done -->"
processFrame (Right frame) ws = do let world    = runAbducer frame ws
                                       log      = outputLog world
                                       (ws', _) = worldState world

                                   putStr log
                                   frame <- getFrame
                                   processFrame frame ws'

getFrame :: IO (Either String Frame)
getFrame = do contents <- getLine
              return (fromXml $ xmlParse "stream" contents)