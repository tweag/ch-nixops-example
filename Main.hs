{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import System.Directory (getHomeDirectory)
import System.FilePath
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
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

-- | If registered as @"logger"@, @logger f@ will print the log to
-- @f@.
logger :: FilePath -> Process ()
logger f = do
  liftIO $ date >>= appendFile f -- Marks the beginning of a section
  forever $ do
    (time,pid,msg) <- expect::Process (String,ProcessId,String)
    liftIO $ appendFile f (format time pid msg)
   where format time pid msg =
           msg ++ " (" ++ show pid ++ " at " ++ time ++ ")\n"
         date = do
           time <- getCurrentTime
           return $ "\n== " ++ formatTime defaultTimeLocale "%c" time ++ " ==\n"

main :: IO ()
main = do
  d <- getHomeDirectory
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  logpid <- forkProcess node $ logger (d </> "hwer.log")
  runProcess node $ reregister "logger" logpid
  (mdo pid1 <- forkProcess node $ ping d pid1 pid2
       pid2 <- forkProcess node $ pong pid1 pid2
       return ())
  runProcess node $ do _ <- expect :: Process ()
                       return ()
