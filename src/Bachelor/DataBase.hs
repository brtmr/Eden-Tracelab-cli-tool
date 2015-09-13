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
    db_traceKey   :: Int,
    db_machines   :: M.HashMap MachineId Int,
    db_processes  :: M.HashMap (MachineId,ProcessId) Int,
    db_threads    :: M.HashMap (MachineId,ThreadId) Int,
    db_connection :: Connection
    }

instance Show DBInfo where
    show dbi =
            "\n\n####BEGIN DB INFO\n\n"
        ++  "Trace ID : "  ++ (show $ db_traceKey  dbi) ++ "\n"
        ++  "Machines:  "  ++ (show $ db_machines  dbi) ++ "\n"
        ++  "Processes: "  ++ (show $ db_processes dbi) ++ "\n"
        ++  "Threads:   "  ++ (show $ db_threads   dbi) ++ "\n"
        ++  "\n\n####END DB INFO\n\n"

-- when starting to parse a new file, we need to create a new connection,
-- and then insert a new trace into our list of traces, then store the trace_id
-- into a new DBInfo value.

insertTraceQuery :: Query
insertTraceQuery = "Insert Into Traces (filename, creation_date)\
    \values( ? , now()) returning trace_id;"

createDBInfo :: FilePath -> IO DBInfo
createDBInfo file = do
    conn <- mkConnection
    traceKey <- head <$> query conn insertTraceQuery (Only file)
    case traceKey of
        Only key -> do
            putStrLn $ show $ (key :: Int)
            return $ DBInfo {
                db_traceKey   = key,
                db_machines   = M.empty,
                db_processes  = M.empty,
                db_threads    = M.empty,
                db_connection = conn
                }
        _       -> error "trace insertion failed"

insertMachineQuery :: Query
insertMachineQuery = "Insert Into Machines(num,trace_id)\
    \values( ? , ? ) returning machine_id;"

insertMachine :: DBInfo -> MachineId-> IO DBInfo
insertMachine dbi mid = do
    let conn     = db_connection dbi
        traceKey = db_traceKey   dbi
    machineKey <- head <$> query conn insertMachineQuery (mid, traceKey)
    case machineKey of
        Only key -> do
            return $ dbi {
                db_machines   = M.insert mid key (db_machines dbi)
                }
        _       -> error "machine insertion failed"

insertProcessQuery :: Query
insertProcessQuery =
    "Insert Into Processes(num,machine_id)\
        \values( ? , ? ) returning process_id;"

insertProcess :: DBInfo -> MachineId -> ProcessId -> IO DBInfo
insertProcess dbi mid pid = do
    let conn       = db_connection dbi
        machineKey = (db_machines dbi) M.! mid
    processKey <- head <$> query conn insertProcessQuery (pid, machineKey)
    case processKey of
        Only key -> do
            return $ dbi {
                db_processes   = M.insert (mid,pid) key (db_processes dbi)
                }
        _       -> error "machine insertion failed"

insertThreadQuery :: Query
insertThreadQuery =
    "Insert into Threads(num, process_id)\
    \values( ? , ? ) returning thread_id;"

insertThread :: DBInfo -> MachineId -> ProcessId -> ThreadId -> IO DBInfo
insertThread dbi mid pid tid = do
    let conn       = db_connection dbi
        processKey = (db_processes dbi) M.! (mid,pid)
    threadKey <- head <$> query conn insertThreadQuery (tid, processKey)
    case threadKey of
        Only key -> do
            return $ dbi {
                db_threads   = M.insert (mid,tid) key (db_threads dbi)
                }
        _       -> error "machine insertion failed"


insertEvent :: DBInfo -> GUIEvent -> IO DBInfo
insertEvent dbi g@(NewMachine mid)        = print dbi >> print g >> insertMachine dbi mid
insertEvent dbi g@(NewProcess mid pid)    = print dbi >> print g >> insertProcess dbi mid pid
insertEvent dbi g@(NewThread mid pid tid) = print dbi >> print g >> insertThread  dbi mid pid tid
insertEvent dbi g@(GUIEvent mtpType start dur state) = do
    print dbi
    print g
    case mtpType of
        Machine mid     -> insertMachineState dbi mid start dur state
        Process mid pid -> insertProcessState dbi mid pid start dur state
        Thread  mid tid -> insertThreadState  dbi mid tid start dur state


insertMachineStateQuery :: Query
insertMachineStateQuery =
    "Insert into machine_events(machine_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertMachineState :: DBInfo -> MachineId -> Timestamp -> Timestamp -> RunState
    -> IO DBInfo
insertMachineState dbi mid start duration state = do
    let conn = db_connection dbi
        machineKey = (db_machines dbi) M.! mid
    execute conn insertMachineStateQuery
        (machineKey, start, duration, stateToInt state)
    return dbi

insertProcessStateQuery :: Query
insertProcessStateQuery =
    "Insert into process_events(process_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertProcessState :: DBInfo -> MachineId -> ProcessId -> Timestamp -> Timestamp
    -> RunState -> IO DBInfo
insertProcessState dbi mid pid start duration state = do
    let conn = db_connection dbi
        processKey = (db_processes dbi) M.! (mid,pid)
    execute conn insertProcessStateQuery
        (processKey, start, duration, stateToInt state)
    return dbi

insertThreadStateQuery :: Query
insertThreadStateQuery =
    "Insert into thread_events(thread_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertThreadState :: DBInfo -> MachineId -> ThreadId -> Timestamp -> Timestamp
    -> RunState -> IO DBInfo
insertThreadState dbi mid tid start duration state = do
    let conn = db_connection dbi
        threadKey = (db_threads dbi) M.! (mid,tid)
    execute conn insertThreadStateQuery
        (threadKey, start, duration, stateToInt state)
    return dbi

