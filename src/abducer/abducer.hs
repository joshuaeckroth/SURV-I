module Main
where

import IO
import Network.Socket
import System.Win32.Process (sleep)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent
import Comm
import Types
import World
import Detection
import Movement

main = do
  s <- initSocket
  loop s
    where loop s = do
            s' <- listenForDetector s
            let world = mkWorld
            waitForCommands s' world
            putStrLn "Shutting down socket."
            sClose s'
            loop s

waitForCommands :: Socket -> World -> IO ()
waitForCommands s world = do
  cmd <- getCommand s
  case cmd of
    CmdNewDetections -> do
             cameraDetections <- getCameraDetections s
             case cameraDetections of
               Left str    -> do { putStrLn str ; return () }
               Right cdets -> do
                            putStrLn "Got detections."
                            let world' = runAbducer cdets world
                            logStatistics world world'
                            putStrLn "Responding with results."
                            respondWithResults s world'
                            putStrLn "Waiting for command."
                            waitForCommands s world'
    CmdQuit          -> do { return () }
    _                -> do { sleep 500 ; waitForCommands s world }

getCameraDetections :: Socket -> IO (Either String CameraDetections)
getCameraDetections s = do
  xml <- getResults s
  return (fromXml $ xmlParse "stream" xml)

logStatistics :: World -> World -> IO ()
logStatistics world world' =
    let (Results dets movs paths)    = buildResults world
        (Results dets' movs' paths') = buildResults world'
        detsDiff  = (length dets') - (length dets)
        movsDiff  = (length movs') - (length movs)
        pathsDiff = (length paths') - (length paths)
    in do
      putStr "Hypotheses: "
      putStr (show $ length dets')
      putStr " ("
      putStr (if (detsDiff > 0) then ("+" ++ (show detsDiff)) else (show detsDiff))
      putStr ") detections, "
      putStr (show $ length movs')
      putStr " ("
      putStr (if (movsDiff > 0) then ("+" ++ (show movsDiff)) else (show movsDiff))
      putStr ") movements, "
      putStr (show $ length paths')
      putStr " ("
      putStr (if (pathsDiff > 0) then ("+" ++ (show pathsDiff)) else (show pathsDiff))
      putStrLn ") paths"

respondWithResults :: Socket -> World -> IO ()
respondWithResults s world = sendResults s $ outputLog world

-- | Execute the abduction
runAbducer :: CameraDetections
           -> World
           -> World
runAbducer cameraDetections world =
    let cleanedWorld = cleanWorld world
        dets         = mkDetections cameraDetections
        movs         = mkMovements dets
    in
      reason $ hypothesize movs $ hypothesize dets cleanedWorld

{--
      world = ((return $ cleanWorld frame ws) >>=
               updateDetections >>=
               recordFrame >>=

               hypothesizeDetections catID >>=
               hypothesizeTracks catID >>=
               hypothesizeSplitTracks catID >>=
               -- constrainDetectionExplainers >>=

               (\ws -> (recordWorldEvent (["Before reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (showConstrainers ws)
                                          ++ ["Tracks:"] ++ (showTracks (IDMap.keys (trackMap ws)) (trackMap ws) (detMap ws) (curFrame ws))
                                         , emptyElem)
                        >> return ws)) >>=
                  
               (\ws -> return ws { mind = (reason (ReasonerSettings False) High (mind ws)) }) >>=

               (\ws -> (recordWorldEvent (["After reasoning:"] ++
                                          ["Mind:"] ++ (showMind $ mind ws)
                                          ++ ["Constrainers:"] ++ (map show $ getConstrainers $ mind ws)
                                          ++ ["Adjusters:"] ++ (map show $ getAdjusters $ mind ws)
                                         , emptyElem)
                        >> return ws)) >>=
                  
               updateTracks >>=
               recordTracks)

--}
