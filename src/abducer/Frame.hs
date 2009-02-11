module Frame where
import Types
import World
import Text.XML.HaXml.Types as HaXml

recordFrame :: WorldState -> World WorldState
recordFrame ws = recordFrame' (curFrame ws) ws
    where
      recordFrame' :: Frame -> WorldState -> World WorldState
      recordFrame' f ws =
          recordWorldEvent (["Frame " ++ show (frameProp frameNumber f) ++
                             ", time = " ++ show (frameProp frameTime f)],
                            emptyElem)
                               >> return (ws { curFrame = f })

-- | Return the detections in the frame with the additional detFrame property added
getDetections :: Frame -> [Detection]
getDetections frame@(Frame _ dets) = map (\(Detection attrs e c) ->
                                          Detection (attrs { detFrame = frame }) e c)
                                     dets

