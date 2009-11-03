module Comm
where

import Network.Socket
import Network.BSD
import Data.Word
import IO

data Command = CmdNewDetections | CmdQuit | CmdUnknown

mkPortNum :: (Integral a) => a -> PortNumber 
mkPortNum p = fromIntegral p

initSocket :: (Integral a) => a -> IO Socket
initSocket port = withSocketsDo $ do
                    proto <- getProtocolNumber "tcp"
                    s <- socket AF_INET Stream proto
                    bindSocket s (SockAddrInet (mkPortNum port) 16777343)
                    return s

listenForDetector :: Socket -> IO Socket
listenForDetector s = do
      listen s 1
      putStrLn "Listening..."
      (s', sock_addr) <- accept s
      putStrLn "Detector connected."
      return s'

getCommand :: Socket -> IO Command
getCommand s = do
  msg <- catch (readUntilNewline s)
         (\e -> return "")
  case msg of
    "NEW DETECTIONS" -> do { return CmdNewDetections }
    "QUIT" -> do { return CmdQuit }
    _ -> do { return CmdUnknown }

readUntilNewline :: Socket -> IO String
readUntilNewline s = do
  str <- recv s 1
  if (str /= "\n") then do
                     rest <- readUntilNewline s
                     return (str ++ rest)
    else do { return "" }

getResults :: Socket -> IO String
getResults s = do
  size <- readUntilNewline s
  msg <- recv s (read size)
  return msg

sendResults :: Socket -> String -> IO ()
sendResults s msg = do
  send s ("NEW RESULTS\n"
          ++ ((show $ 1 + (length msg)) ++ "\n") {- 1+ because of newline after msg is sent -}
          ++ msg
          ++ "\n")
  return ()
