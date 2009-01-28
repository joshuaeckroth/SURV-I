module Frame where
import Types
import World
import Text.XML.HaXml.Types as HaXml

recordFrame :: WorldState -> World WorldState
recordFrame ws = recordFrame' (frame ws) ws
    where
      recordFrame' :: Frame -> WorldState -> World WorldState
      recordFrame' f@(Frame attrs _) ws =
          recordWorldEvent (["Frame " ++ show (frameNumber attrs) ++ ", time = " ++ show (frameTime attrs)],
                            emptyElem)
                               >> return (ws { frame = f })

getAcquisitions :: Frame -> [Acquisition]
getAcquisitions (Frame _ acqs) = filter (\acq -> (area acq) > 10.0) acqs
