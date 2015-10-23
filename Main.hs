{-# LANGUAGE RecursiveDo #-}

module Main where

import System.Directory (getHomeDirectory)
import System.FilePath
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

-- | @ping d pid1 pid@ sends a message to @pid@ and waits for a
-- reply. @d@ and @pid1@ are temporary logging arguments.
ping :: FilePath -> ProcessId -> ProcessId -> Process ()
ping d pid1 pid2 = do
  say $ "Dir: " ++ d
  say $ show pid1
  send pid2 "hello"
  hello <- expect :: Process String
  say $ "Received: " ++ hello
  liftIO $ appendFile (d </> "plop.txt" ) (hello++"\n")

-- | @pong pid1 pid2$ waits for a message and replies to
-- @pid1@. @pid2@ is a temporary logging argument.
pong :: ProcessId -> ProcessId -> Process ()
pong pid1 pid2 = do
  say $ show pid2
  msg <- expect :: Process String
  send pid1 $ "You said: " ++ msg

main :: IO ()
main = do
  d <- getHomeDirectory
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  (mdo pid1 <- forkProcess node $ ping d pid1 pid2
       pid2 <- forkProcess node $ pong pid1 pid2
       return ())
  runProcess node $ do _ <- expect :: Process ()
                       return ()
