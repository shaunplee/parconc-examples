{-# LANGUAGE RecordWildCards #-}
module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Concurrent.STM
import Control.Distributed.Process

import qualified Data.Map as Map
import Data.Map (Map)

type Key   = String
type Value = String

data Database = Database { kvStore :: TVar (Map Key Value), spid :: ProcessId }

createDB :: [NodeId] -> Process Database
createDB nodes = do
  pid <- getSelfPid
  liftIO $ do
    kvs <- newTVarIO Map.empty
    return Database { kvStore = kvs, spid = pid}

set :: Database -> Key -> Value -> Process ()
set Database{..} k v = liftIO $ atomically $ do
  kvs <- readTVar kvStore
  writeTVar kvStore (Map.insert k v kvs)

get :: Database -> Key -> Process (Maybe Value)
get db k = liftIO $ atomically $ do
  kvs <- readTVar (kvStore db)
  return $ Map.lookup k kvs

rcdata :: RemoteTable -> RemoteTable
rcdata = id
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
