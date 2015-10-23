{-# LANGUAGE RecursiveDo #-}

module Main where

import System.Directory (getHomeDirectory)
import System.FilePath
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
  d <- getHomeDirectory
  putStrLn $ "Dir: " ++ d
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  (mdo pid1 <- forkProcess node $ do
         liftIO $ print pid1
         send pid2 "hello"
         hello <- expect :: Process String
         liftIO $ do putStrLn $ "Received: " ++ hello
                     appendFile (d </> "plop.txt") hello
       pid2 <- forkProcess node $ do
         liftIO $ print pid2
         msg <- expect :: Process String
         send pid1 $ "You said: " ++ msg
       return ())
  runProcess node $ do _ <- expect :: Process ()
                       return ()
