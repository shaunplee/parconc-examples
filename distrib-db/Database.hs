{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Distributed.Process.Closure
import Control.Monad (forever)

import Data.Binary (Binary)
import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics

import Debug.Trace (trace)

type Key   = String
type Value = String

type Database = ProcessId

type Client = ProcessId

data DbMessage = Set Client Key Value | Get Client Key | Response (Maybe Value)
    deriving ( Typeable, Generic )

instance Binary DbMessage

dbHandleMessage :: TVar (Map Key Value) -> DbMessage -> Process ()
dbHandleMessage kvStore (Set _ k v) = liftIO $ atomically $ do
  kvs <- readTVar kvStore
  writeTVar kvStore (Map.insert k v kvs)
dbHandleMessage kvStore (Get c k) = do
  v <- liftIO $ atomically $ do
    kvs <- readTVar kvStore
    return $ Map.lookup k kvs
  send c (Response v)

dbServer :: Process ()
dbServer = do
  kvs <- liftIO $ newTVarIO Map.empty
  forever $ do
    m <- expect :: Process DbMessage
    dbHandleMessage kvs m

remotable ['dbServer]

createDB :: [NodeId] -> Process Database
createDB nodes = do
  node <- getSelfNode
  pid <- spawn node $(mkStaticClosure 'dbServer)
  return pid

set :: Database -> Key -> Value -> Process ()
set db k v = do
  pid <- getSelfPid
  send db (Set pid k v)
  return ()

get :: Database -> Key -> Process (Maybe Value)
get db k = do
  pid <- getSelfPid
  send db (Get pid k)
  Response resp <- expect
  return resp

rcdata :: RemoteTable -> RemoteTable
rcdata = Database.__remoteTable
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
