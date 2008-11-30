import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Abducer
import Types
import World

main =
    do
      let ws = newWorldState
      args <- System.getArgs
      (Frames frames) <- if (length args) > 0 then
                             fReadXml $ head args :: IO Frames
                         else
                             hGetXml stdin :: IO Frames
      runAbducer frames ws
