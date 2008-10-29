
import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ

import AcquisitionTypes

main =
    do
        args <- System.getArgs
        frames <- fReadXml $ head args :: IO Frames
        putStrLn $ show frames



