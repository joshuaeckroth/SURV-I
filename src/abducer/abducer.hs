module Main
where

import IO
import Network.Socket
import System.Win32.Process (sleep)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.XmlContent
import Data.List (nub)
import Comm
import Types
import World
import Detection
import Movement
import Path
import qualified WrappedInts.IDMap as IDMap
import qualified WrappedInts.IDSet as IDSet
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
    let (Results _ (Accepted dets movs paths) (Rejected rdets rmovs rpaths)) = buildResults world
        (Results _ (Accepted dets' movs' paths') (Rejected rdets' rmovs' rpaths')) = buildResults world'
        detsDiff    = (length dets') - (length dets)
        rdetsDiff   = (length rdets') - (length rdets)
        movsDiff    = (length movs') - (length movs)
        rmovsDiff   = (length rmovs') - (length rmovs)
        pathsDiff   = (length paths') - (length paths)
        rpathsDiff  = (length rpaths') - (length rpaths)
        numEntities = IDMap.size (entityMap world')
    in do
      putStr "Hypotheses: "
      putStr (show $ length dets')
      putStr "("
      putStr (if (detsDiff > 0) then ("+" ++ (show detsDiff)) else (show detsDiff))
      putStr ") detections - "
      putStr (show $ length rdets')
      putStr "("
      putStr (if (rdetsDiff > 0) then ("+" ++ (show rdetsDiff)) else (show rdetsDiff))
      putStr ") rejected; "
      putStr (show $ length movs')
      putStr "("
      putStr (if (movsDiff > 0) then ("+" ++ (show movsDiff)) else (show movsDiff))
      putStr ") movements - "
      putStr (show $ length rmovs')
      putStr "("
      putStr (if (rmovsDiff > 0) then ("+" ++ (show rmovsDiff)) else (show rmovsDiff))
      putStr ") rejected; "
      putStr (show $ length paths')
      putStr "("
      putStr (if (pathsDiff > 0) then ("+" ++ (show pathsDiff)) else (show pathsDiff))
      putStr ") paths - "
      putStr (show $ length rpaths')
      putStr "("
      putStr (if (rpathsDiff > 0) then ("+" ++ (show rpathsDiff)) else (show rpathsDiff))
      putStrLn ") rejected"
      putStrLn ("Number of entities in entity map: " ++ (show numEntities))

respondWithResults :: Socket -> World -> IO ()
respondWithResults s world = sendResults s $ outputLog world

-- | Execute the abduction
runAbducer :: CameraDetections
           -> World
           -> World
runAbducer cameraDetections world =
    let cleanedWorld   = cleanWorld world
        emap           = entityMap cleanedWorld
        allHyps        = allHypotheses cleanedWorld
        existingDets   = gatherEntities emap allHyps :: [Detection]
        existingMovs   = gatherEntities emap allHyps :: [Movement]
        existingPaths  = gatherEntities emap allHyps :: [Path]
        dets           = mkDetections cameraDetections
        emap'          = foldl addToEntityMap emap (map (\(Hyp {entity = det}) ->
                                                         (extractDetHypId det, det)) dets)
        movs           = mkMovements $ nub (existingDets ++ (extractEntities dets))
        -- filter out movement hyps that have already been posed
        -- (note that a hyp's ID hash uniquely identifies the hyp by hashing its components)
        newMovs        = filter (\(Hyp {hypId = hypId}) -> not $ IDMap.member hypId emap) movs
        emap''         = foldl addToEntityMap emap' (map (\(Hyp {entity = mov}) ->
                                                          (extractMovHypId mov, mov)) newMovs)
        paths          = mkPaths emap'' existingPaths $ nub (existingMovs ++ (extractEntities movs))
        -- filter out duplicate paths
        newPaths       = filter (\(Hyp {hypId = hypId}) -> not $ IDMap.member hypId emap) paths
        -- filter out subpaths among newPaths and existing paths
        nonSubPaths    = let allPaths = (extractEntities newPaths) ++ (gatherEntities emap allHyps)
                             subPaths = findSubPaths (extractEntities newPaths) allPaths
                         in filter (\(Hyp {entity = path}) -> not $ elem path subPaths) newPaths
    in
      reason $ updateConflictingPaths $ removeSubPaths $ hypothesize nonSubPaths $
             hypothesize newMovs $ hypothesize dets cleanedWorld
