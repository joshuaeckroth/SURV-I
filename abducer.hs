
import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ

import AcquisitionTypes
import AcquisitionFuncs

main =
    do
        args <- System.getArgs
        frames <- fReadXml $ head args :: IO Frames
        putStrLn $ processAll frames

processAll :: Frames -> String
processAll (Frames frames) = processFrames frames

processFrames :: [Frame] -> String
processFrames [] = ""
processFrames ((Frame attrs acqs):frames) = processAcquisitions acqs ++ "\n" ++ processFrames frames

processAcquisitions :: [Acquisition] -> String
processAcquisitions [] = ""
processAcquisitions (x:[]) = "odd case"
processAcquisitions (x:y:xs) = (show $ distance x y) ++ "\n" ++ processAcquisitions xs

