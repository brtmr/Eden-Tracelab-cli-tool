 {-# LANGUAGE OverloadedStrings #-}
module Bachelor.DataBase where

{-
 - Provides a interface for reading/writing RTS State Events to and from
 - a database.
 - -}

--instance IOEventData where

import Bachelor.Types
import Database.PostgreSQL.Simple
import GHC.RTS.Events
import qualified Data.HashMap.Strict as M

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
    connectPassword = "bohCh0mu"
    }

mkConnection = connect myConnectInfo

-- the information necessary for entering events into the database.
-- because the database should be able to contain multiple traces,
-- we keep a set of keys to uniquely identify the machines, processes and
-- threads within the current trace.

data DBInfo = DBInfo {
    db_machines   :: M.HashMap MachineId Int,
    db_processes  :: M.HashMap (MachineId,ProcessId) Int,
    db_threads    :: M.HashMap (MachineId,ProcessId,ThreadId) Int,
    db_connection :: Connection
    }
