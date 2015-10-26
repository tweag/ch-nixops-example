
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
ping :: ProcessId -> Process ()
ping pid2 = do
  mypid <- getSelfPid
  send pid2 (mypid,"hello")
  hello <- expect :: Process String
  say $ "Received: \"" ++ hello ++ "\" back"

-- | 'pong' waits for a message pair with a 'ProcessId'
-- and replies to the corresponding process.
pong :: Process ()
pong = do
  (rpid,msg) <- expect :: Process (ProcessId,String)
  say $ "Received: \"" ++ msg ++"\""
  send rpid $ "You said: " ++ msg

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
  runProcess node $ say $ "Dir: " ++ d
  pongpid <- forkProcess node $ pong
  _ <- forkProcess node $ ping pongpid
  runProcess node $ expect
