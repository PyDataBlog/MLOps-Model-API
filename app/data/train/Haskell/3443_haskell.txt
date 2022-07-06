module Main where

import Network
import System.IO (hPutStrLn, hClose, Handle)
import Control.Concurrent (forkIO)


main :: IO ()
main = startServer >>= handleConnections qotdService


startServer :: IO Socket
startServer = listenOn $ PortNumber 17


handleConnections :: (Handle -> IO ()) -> Socket -> IO ()
handleConnections handler socket = do
  putStrLn "Received connection..."
  (handle, _, _) <- accept socket
  _ <- forkIO $ handler handle >> hClose handle
  handleConnections handler socket


qotdService :: Handle -> IO ()
qotdService file = hPutStrLn file "Hello world"
