{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.ByteString.Char8 as ByteStringChar
import qualified Network.Transport as NT
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node


-- | Names of the machines on the network
helloworlder :: String
helloworlder = "helloworlder"

ponger :: String
ponger = "ponger"

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

registerAsLogger :: ProcessId -> Process ()
registerAsLogger pid =
  reregister "logger" pid

--
-- Template Haskell boilerplate for serialisation
--

remotable ['ping, 'pong, 'logger, 'registerAsLogger]

--
-- /boilerplate
--

-- | Port on which the nodes of the cluster will listen
port :: String
port = "10501"

-- | @makeNodeId addr@ creates a 'NodeId' to represent the process
-- which runs on the machine at address @addr@.
makeNodeId :: String -> NodeId
makeNodeId addr =
  NodeId . NT.EndPointAddress . ByteStringChar.pack $ addr ++ ":" ++ port ++ ":0"

-- | 'NodeId' of the nodes on the cluster
helloworlderid :: NodeId
helloworlderid = makeNodeId helloworlder

pongerid :: NodeId
pongerid = makeNodeId ponger



dispatch :: [String] -> RemoteTable -> IO ()
dispatch [] rtable = do
  d <- getHomeDirectory
  Right t <- createTransport helloworlder port defaultTCPParameters
  node <- newLocalNode t rtable
  runProcess node $ do
    logpid <- spawn helloworlderid $ $(mkClosure 'logger) (d </> "hwer.log")
    registerAsLogger logpid
    say $ "Dir: " ++ d
    () <- call $(functionTDict 'registerAsLogger) pongerid $ $(mkClosure 'registerAsLogger) logpid
    pongpid <- spawn pongerid $ $(mkStaticClosure 'pong)
    say "pong spawned"
    _ <- spawn helloworlderid $ $(mkClosure 'ping) pongpid
    say "hwer spawned"
    expect
dispatch ["--node"] rtable = do
  Right t <- createTransport ponger port defaultTCPParameters
  node <- newLocalNode t rtable
  runProcess node expect
dispatch _ _ = do
  ioError $ userError "Invalid arguments"

main :: IO ()
main = do
  args <- getArgs
  dispatch args rtable
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
