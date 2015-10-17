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
import qualified Data.ByteString as B

--the number of events to be commited at once
bufferlimit = 1000

mkConnection = do
    connectionString <- B.readFile "pq.conf"
    conn <- connectPostgreSQL connectionString
    _ <- execute_ conn "SET SESSION synchronous_commit TO off"
    return conn

-- the information necessary for entering events into the database.
-- because the database should be able to contain multiple traces,
-- we keep a set of keys to uniquely identify the machines, processes and
-- threads within the current trace.

data DBInfo = DBInfo {
    db_threadbuffer  :: [(MachineId, ThreadId,  Timestamp, Timestamp, RunState)],
    db_processbuffer :: [(MachineId, ProcessId, Timestamp, Timestamp, RunState)],
    db_machinebuffer :: [(MachineId, Timestamp, Timestamp, RunState)],
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

insertTrace :: Connection -> FilePath -> IO DBInfo
insertTrace conn file = do
    traceKey <- head <$> query conn insertTraceQuery (Only file)
    case traceKey of
        Only key -> do
            return $ DBInfo {
                db_processbuffer = [],
                db_threadbuffer  = [],
                db_machinebuffer = [],
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
insertEvent dbi g@(NewMachine mid)        = insertMachine dbi mid
insertEvent dbi g@(NewProcess mid pid)    = insertProcess dbi mid pid
insertEvent dbi g@(NewThread mid pid tid) = insertThread  dbi mid pid tid
insertEvent dbi g@(GUIEvent mtpType start dur state) =
    case mtpType of
        Machine mid -> case ((length $ db_machinebuffer dbi) >= bufferlimit) of
            True  -> do
                --putStrLn "inserting Machine events"
                insertMachineState dbi
            False -> return dbi {
                db_machinebuffer = (mid,start,dur,state) : db_machinebuffer dbi
                }
        Process mid pid -> case ((length $ db_processbuffer dbi) >= bufferlimit) of
            True  -> do
                --putStrLn "inserting Process events"
                insertProcessState dbi
            False -> return dbi {
                db_processbuffer = (mid,pid,start,dur,state) : db_processbuffer dbi
                }
        Thread  mid tid -> case ((length $ db_threadbuffer dbi) >= bufferlimit) of
            True  -> do
                --putStrLn "inserting Thread events"
                insertThreadState dbi
            False -> return dbi {
                db_threadbuffer = (mid,tid,start,dur,state) : db_threadbuffer dbi
                }

finalize :: DBInfo -> IO DBInfo
finalize dbi = do
    dbi <- insertMachineState dbi
    dbi <- insertProcessState dbi
    dbi <- insertThreadState  dbi
    return dbi

insertMachineStateQuery :: Query
insertMachineStateQuery =
    "Insert into machine_events(machine_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertMachineState :: DBInfo -> IO DBInfo
insertMachineState dbi = do
    let conn = db_connection dbi
        inlist = map (\(mid,start,duration,state) ->
            ((db_machines dbi) M.! mid, start, duration, stateToInt state))
                $ db_machinebuffer dbi
    executeMany conn insertMachineStateQuery inlist
    return dbi {db_machinebuffer = []}

insertProcessStateQuery :: Query
insertProcessStateQuery =
    "Insert into process_events(process_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertProcessState :: DBInfo -> IO DBInfo
insertProcessState dbi = do
    let conn = db_connection dbi
        inlist = map (\(mid,pid,start,duration,state) ->
            ((db_processes dbi) M.! (mid,pid), start, duration, stateToInt state))
                $ db_processbuffer dbi
    executeMany conn insertProcessStateQuery inlist
    return dbi {db_processbuffer = []}

insertThreadStateQuery :: Query
insertThreadStateQuery =
    "Insert into thread_events(thread_id, starttime, duration, state)\
    \values( ? , ? , ? , ? );"

insertThreadState :: DBInfo -> IO DBInfo
insertThreadState dbi = do
    let conn = db_connection dbi
        inlist = map (\(mid,tid,start,duration,state) ->
            ((db_threads dbi) M.! (mid,tid), start, duration, stateToInt state))
                $ db_threadbuffer dbi
    executeMany conn insertThreadStateQuery inlist
    return dbi {db_threadbuffer = []}

