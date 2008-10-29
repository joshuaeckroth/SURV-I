
import IO
import System
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.PrettyPrint.HughesPJ

main =
    do
        args <- System.getArgs
        doc <- getContents
        putStrLn $ render $ document $ xmlParse (head args) doc

