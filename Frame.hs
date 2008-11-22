module Frame where
import Types
import World
import Text.XML.HaXml.Types as HaXml

recordFrame :: Frame -> WorldState -> World WorldState
recordFrame (Frame attrs _) ws =
    recordWorldEvent ([show (frameNumber attrs)], HaXml.Elem "Frame" [] []) >> return ws

getAcquisitions :: Frame -> [Acquisition]
getAcquisitions (Frame _ acqs) = acqs
