{-# LANGUAGE TemplateHaskell, CPP #-}
module DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import System.Environment

#if MIN_VERSION_network(2,7,0)
import Network.Socket hiding (shutdown, defaultPort)
#else
import Network.Socket hiding (shutdown)
#endif

import Language.Haskell.TH

distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable

  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startMaster backend master
    [ "slave" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend
    [ "slave", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startSlave backend
    [ "slave", host, port ] -> do
      backend <- initializeBackend host port rtable
      startSlave backend

defaultHost = "localhost"
defaultPort = "44444"
