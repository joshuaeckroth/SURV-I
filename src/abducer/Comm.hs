module Comm
where

import qualified Data.ByteString.Char8 as B
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
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
         (\e -> return B.empty)
  case (B.unpack msg) of
    "NEW DETECTIONS" -> do { return CmdNewDetections }
    "QUIT" -> do { return CmdQuit }
    _ -> do { return CmdUnknown }

readUntilNewline :: Socket -> IO B.ByteString
readUntilNewline s = do
  str <- recv s 1
  if (str /= (B.pack "\n")) then
      do
        rest <- readUntilNewline s
        return (str `B.append` rest)
    else do { return B.empty }

getResults :: Socket -> IO String
getResults s = do
  sizeString <- readUntilNewline s
  let sizeMaybe = B.readInt sizeString
  case sizeMaybe of
    (Just (size, _)) -> do msg <- getResultChunk s size
                           return $ B.unpack msg
    Nothing          -> do return ""

sendResults :: Socket -> String -> IO ()
sendResults s msg = do
  putStrLn msg
  send s $ B.pack ("NEW RESULTS\n" ++ ((show $ 1 + (length msg)) ++ "\n")){- 1+ because of newline after msg is sent -}
  sendResultChunk s (B.pack msg)
  send s (B.pack "\n")
  return ()

sendResultChunk :: Socket -> B.ByteString -> IO ()
sendResultChunk s msg =
    if B.null msg then do return ()
      else do
        let chunk = B.take 4000 msg
            rest  = B.drop 4000 msg
        send s chunk
        sendResultChunk s rest

getResultChunk :: Socket -> Int -> IO B.ByteString
getResultChunk s left
    | left <= 0 = return B.empty
    | otherwise = do chunk <- recv s (4000)
                     rest  <- getResultChunk s (left - 4000)
                     return $ chunk `B.append` rest
