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
import Control.Applicative

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
    db_traceId    :: Int,
    db_machines   :: M.HashMap MachineId Int,
    db_processes  :: M.HashMap (MachineId,ProcessId) Int,
    db_threads    :: M.HashMap (MachineId,ProcessId,ThreadId) Int,
    db_connection :: Connection
    }

-- when starting to parse a new file, we need to create a new connection,
-- and then insert a new trace into our list of traces, then store the trace_id
-- into a new DBInfo value.

createDBInfo :: FilePath -> IO ()
createDBInfo file = do
    conn <- mkConnection
    traceId <- head <$> query conn ("Insert Into Traces (filename, creation_date) values( ? , now()) returning trace_id;") (Only file)
    case traceId of
        Only id -> putStrLn $ show $ (id :: Int)

-- insertion functions for different Events
insertEvent :: DBInfo -> Event -> IO DBInfo
insertEvent dbi (Event ts spec) =
    case spec of
        CreateMachine realtime m_id -> do
            return dbi
        -- events not implemented
        _                  -> do
            return dbi
