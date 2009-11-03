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
import Path
import Debug.Trace

main = do
  outSocket <- initSocket 10000
  inSocket <- initSocket 10001
  loop (outSocket, inSocket)
    where loop (outSocket, inSocket) = do
            outSocket' <- listenForDetector outSocket
            inSocket'  <- listenForDetector inSocket
            let world = mkWorld
            waitForCommands (outSocket', inSocket') world
            putStrLn "Shutting down sockets."
            sClose outSocket'
            sClose inSocket'
            loop (outSocket, inSocket)

waitForCommands :: (Socket, Socket) -> World -> IO ()
waitForCommands (outSocket, inSocket) world = do
  cmd <- getCommand inSocket
  case cmd of
    CmdNewDetections -> do
             cameraDetections <- getCameraDetections inSocket
             case cameraDetections of
               Left str    -> do { putStrLn str ; return () }
               Right cdets -> do
                            putStrLn "Got detections."
                            let world' = runAbducer cdets world
                            logStatistics world world'
                            putStrLn "Responding with results."
                            respondWithResults outSocket world'
                            putStrLn "Waiting for command."
                            waitForCommands (outSocket, inSocket) world'
    CmdQuit          -> do { return () }
    _                -> do { sleep 500 ; waitForCommands (outSocket, inSocket) world }

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
        paths        = mkPaths movs
    in
      reason $ hypothesize paths $ hypothesize movs $ hypothesize dets cleanedWorld

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
