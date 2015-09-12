{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.Parsers
import Bachelor.Types
import Control.Applicative
import Control.Lens
import Data.List
import Data.Map.Lens
import Data.Maybe
import GHC.RTS.Events hiding (machine)
import qualified Bachelor.DataBase as DB
import qualified Bachelor.TinyZipper as TZ
import qualified Bachelor.Util as U
import qualified Data.Array.IArray as Array
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Directory as Dir
import qualified System.IO as IO

-- The Parsing state for a specific capability. It contains the Lazy ByteString
-- consumed up to the current event, and the last parsed Event.
type CapState = (LB.ByteString, Maybe Event)

-- The state that a parser of a single EventLog carries.
data ParserState = ParserState {
    _p_caps     :: CapState,      -- the 'system' capability.
    _p_cap0     :: CapState,      -- capability 0.
    _p_rtsState :: RTSState,      -- the inner state of the runtime
    _p_pt       :: ParserTable    -- event types and their parsers,
                                  -- generated from the header.
        }

$(makeLenses ''ParserState)

instance Show ParserState where
    show (ParserState (bss,es) (bs0,e0) rts pt) =
           "\n\n#####BEGIN PARSER STATE\n\n"
        ++ "System Capability: " ++ "\n"
        ++ "    " ++ (show $ LB.take 10 $ bss) ++ "\n"
        ++ (show es) ++ "\n"
        ++ "Capability 0: " ++ "\n"
        ++ "    " ++ (show $ LB.take 10 $ bs0) ++ "\n"
        ++ (show e0) ++ "\n"
        ++ "Run-Time State: "
        ++ (show rts)
        ++ "\n\n#### END PARSER STATE\n\n"

-- the state that the overall parser keeps.
-- contains the parser information for every single *.eventlog file,
-- as well as the DataBase connection.
-- if time permits, this might be extended to contain a message queue,
-- containing open messages that have not yet been committed to the
-- database.
data MultiParserState = MultiParserState {
    _machineTable :: M.HashMap MachineId ParserState, -- Each machine has its
                                                      -- own ParserState.
    _con    :: DB.DBInfo -- with a global DataBase connection.
    } deriving Show

$(makeLenses ''MultiParserState)

-- paths:
testdir = "/home/basti/bachelor/traces/mergesort_small/"

-- instead of parsing a single *.eventlog file, we want to parse a *.parevents
-- file, which is a zipfile containing a set of *.eventlog files, one for
-- every Machine used.
-- because reading from a zipfile lazily seems somewhat troubling, we will
-- instead unzip all files beforehand and then read them all as single files.
run :: FilePath -> IO()
run dir = do
    -- filter the directory contents into eventlogs.
    paths <- filter (isSuffixOf ".eventlog") <$> Dir.getDirectoryContents dir
        -- prepend the directory.
    let files = map (\x -> dir ++ x) paths
        -- extract the machine number.
        mids  = map extractNumber paths
    -- connect to the DataBase, and enter a new trace, with the current
    -- directory and time.
    dbi <- DB.createDBInfo dir
    -- create a parserState for each eventLog file.
    pStates <- zip mids <$> mapM createParserState files
    -- create the multistate that encompasses all machines.
    let mState = MultiParserState {
        _machineTable = M.fromList pStates,
        _con = dbi}
    print mState
    -- for testing purposes only: test the first machine
    let m1 = fromJust $ (mState^.machineTable^.(at 2))
    parseSingleEventLog dbi 2 m1

{-
 - each eventLog file has the number of the according machine (this pe) stored
 - in the filename as base_file#xxx.eventlog, where xxx is the number
 - that will also be the later MachineId
 -}
extractNumber :: String -> MachineId
extractNumber str = read $ reverse $ takeWhile (/= '#') $ drop 9 $ reverse str

createParserState :: FilePath -> IO ParserState
createParserState fp = do
    let mid = extractNumber fp
    bs <- LB.readFile fp
    case AL.parse headerParser bs of
        AL.Done bsrest header -> do
            let pt     = mkParserTable header
                bsdata = LB.drop 4 bsrest --'datb'
            return ParserState {
                _p_caps     = getFirstCapState bsdata pt 0xFFFF,
                _p_cap0     = getFirstCapState bsdata pt 0,
                _p_rtsState = makeRTSState mid,
                _p_pt       = pt
                }
        _                     -> error $ "failed parsing header of file " ++ fp

getFirstCapState :: LB.ByteString -> ParserTable -> Capability -> CapState
getFirstCapState bs pt cap =
    case AL.parse (parseSingleEvent pt cap) bs of
        AL.Done bsrest res -> (bsrest,res)
        _                  -> error $ "failed parsing the first event for cap " ++ (show cap) ++ "\n"
                                      ++ (show $ LB.take 20 bs)

{-
 - This is the main function for parsing a single event log and storing
 - the events contained within into the database.
 - -}
parseSingleEventLog :: DB.DBInfo -> MachineId -> ParserState -> IO()
-- event blocks need to be skipped without handling them.
-- System EventBlock
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just (Event _ EventBlock{})))
    _ rts pt) = parseSingleEventLog dbi mid $ parseNextEventSystem pstate
-- Cap 0 EventBlock
parseSingleEventLog dbi mid pstate@(ParserState
  _ (bs0,e0@(Just (Event _ EventBlock{})))
    rts pt) = parseSingleEventLog dbi mid $ parseNextEventNull pstate
-- both capabilies still have events left. return the earlier one.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just (Event tss specs)))
    (bs0,ev0@(Just (Event ts0 spec0)))
    rts pt) = if (tss < ts0)
        then do
            putStrLn "System cap."
            print evs
            parseSingleEventLog dbi mid $ parseNextEventSystem pstate
        else do
            putStrLn "cap. 0"
            print ev0
            parseSingleEventLog dbi mid $ parseNextEventNull pstate
-- no more system events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@(Just (Event ts0 spec0)))
    rts pt) = do
            putStrLn "cap. 0"
            print ev0
            parseSingleEventLog dbi mid $ parseNextEventNull pstate
-- no more cap0 events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just (Event tss specs)))
    (bs0,ev0@Nothing)
    rts pt) = do
            putStrLn "System cap."
            print evs
            parseSingleEventLog dbi mid $ parseNextEventSystem pstate
-- no more events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@Nothing)
    rts pt) = return ()

-- takes the parser state of a capability
-- replaces the event with the next one in the bytstring.
parseNextEvent :: CapState -> ParserTable -> Capability -> CapState
parseNextEvent (bs,e) pt cap =
    case AL.parse (parseSingleEvent pt cap) bs of
        AL.Done bsrest res -> (bsrest,res)
        _                  -> error $ "Failing to parse event: \n\n"
                                        ++ "Capability: "     ++ (show $ cap)
                                            ++ "\n"
                                        ++ "Previous Event: " ++ (show $ e)
                                            ++ "\n"
                                        ++ (show $ LB.take 100 $ bs)

-- specific functions to parse the next event for the system cap and the
-- 1st capability.
-- short function definition through lens magic.
parseNextEventSystem :: ParserState -> ParserState
parseNextEventSystem pstate =
                p_caps %~ (\s -> parseNextEvent s (pstate^.p_pt) 0xFFFF) $ pstate

parseNextEventNull :: ParserState -> ParserState
parseNextEventNull pstate =
                p_cap0 %~ (\s -> parseNextEvent s (pstate^.p_pt) 0) $ pstate
{-
    Handlers for the different EventTypes.
    Some do not create GUIEvents, so they just return the new ParserState
    Some do create GUIEvents, so they return (ParserState,[GUIEvent])
-}

{- when Events are handled, we need to know from which Eventlog they where
 - sourced, so they are annotated with additional Information:  -}
data AssignedEvent = AssignedEvent {
    _event :: Event,
    _machine :: MachineId,
    _cap :: Int
    }

$(makeLenses ''AssignedEvent)

type HandlerType = RTSState -> AssignedEvent -> (RTSState,[GUIEvent])

handleEvent :: HandlerType
handleEvent rts aEvent@(AssignedEvent event@(Event ts spec) mid cap) =
    case spec of
        CreateMachine mid realtime  ->
            let newMachine = MachineState {
                _m_state     = Idle,
                _m_timestamp = ts,
                _m_pRunning  = 0,
                _m_pRunnable = 0,
                _m_pBlocked  = 0,
                _m_pTotal    = 0
                }
                creationEvent = NewMachine mid
            in (set rts_machine newMachine $ rts, [creationEvent])
        CreateProcess pid ->
            let newProcess = ProcessState {
                _p_parent    = mid,
                _p_state     = Runnable,
                _p_timestamp = ts,
                _p_tRunning  = 0,
                _p_tRunnable = 0,
                _p_tBlocked  = 0,
                _p_tTotal    = 0
                }
                creationEvent = NewProcess mid pid
            in (set (rts_processes.(at pid)) (Just newProcess) $ rts, [])
        AssignThreadToProcess tid pid ->
            let newThread = ThreadState {
                _t_parent      = pid,
                _t_state       = Runnable,
                _t_timestamp   = ts
                }
                creationEvent = NewThread mid tid
            in (set (rts_threads.(at tid)) (Just newThread) $ rts, [])
        RunThread tid ->
            let oldThread           = (rts^.rts_threads) M.! tid
                oldState            = oldThread^.t_state
                pid                 = oldThread^.t_parent
                oldProcess          = (rts^.rts_processes) M.! pid
                (newThread,tEvent)  = setThreadState mid tid oldThread ts
                    oldState
                (newProcess,pEvent) = updateThreadCountAndProcessState
                    mid pid ts oldProcess (Just oldState) (Just Running)
                oldProcessState     = oldProcess^.p_state
                newProcessState     = newProcess^.p_state
                (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                    (rts^.rts_machine) (Just oldProcessState)
                    (Just newProcessState)
                rts' = set rts_machine newMachine $
                       set (rts_threads.(at tid))   (Just newThread) $
                       set (rts_processes.(at pid)) (Just newProcess) $ rts
            in (rts', mList [tEvent, pEvent])
        StopThread tid _ ->
            let oldThread           = (rts^.rts_threads) M.! tid
                oldState            = oldThread^.t_state
                pid                 = oldThread^.t_parent
                oldProcess          = (rts^.rts_processes) M.! pid
                (newThread,tEvent)  = setThreadState mid tid oldThread ts
                    oldState
                (newProcess,pEvent) = updateThreadCountAndProcessState
                    mid pid ts oldProcess (Just oldState) (Just Running)
                oldProcessState     = oldProcess^.p_state
                newProcessState     = newProcess^.p_state
                (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                    (rts^.rts_machine) (Just oldProcessState)
                    (Just newProcessState)
                rts' = set rts_machine newMachine $
                       set (rts_threads.(at tid))   (Just newThread) $
                       set (rts_processes.(at pid)) (Just newProcess) $ rts
            in (rts', mList [tEvent, pEvent])
        _ -> (rts,[])


{-
 - We often have to deal with lists of type [Maybe GUIEvent], and want to
 - filter the actual events.
 - -}
mList = (map fromJust).(filter isJust)

setThreadState :: MachineId -> ThreadId -> ThreadState -> Timestamp -> RunState
                    -> (ThreadState, Maybe GUIEvent)
setThreadState mid tid t ts state
    | t^.t_state == state = (t,Nothing)
    | otherwise = (t {
        _t_state       = state,
        _t_timestamp   = ts
        }, Just $ GUIEvent {
            mtpType   = Thread mid tid,
            startTime = t^.t_timestamp,
            duration  = ts - t^.t_timestamp,
            state     = t^.t_state
            })

updateProcessCountAndMachineState :: MachineId
                                    -> Timestamp
                                    -> MachineState
                                    -> (Maybe RunState)
                                    -> (Maybe RunState)
                                    -> (MachineState, Maybe GUIEvent)
updateProcessCountAndMachineState mid ts m oldState newState  = let
    m'  = updateProcessCount m oldState newState
    m'' = setMachineState m'
    in if _m_state m == _m_state m''
        then (m'',Nothing)
        else (set m_state (_m_state m'')$ set m_timestamp ts $ m'', Just $ GUIEvent {
            mtpType   = Machine mid,
            startTime = _m_timestamp m,
            duration  = ts - _m_timestamp m,
            state     = _m_state m
            })

{- takes a state transition within a Process, and the parent MachineId
 - and updates the Machine State accordingly. -}

updateProcessCount :: MachineState -> (Maybe RunState) -> (Maybe RunState)
                      -> MachineState
updateProcessCount m oldState newState
    | oldState == newState = m
    | (oldState == Just Idle) || (newState == Just Idle) = m
    | otherwise = let m' = case oldState of
                        --decrement the old state counter, or insert the event.
                        (Just Running)  -> m_pRunning  %~ (\x -> x-1) $ m
                        (Just Blocked)  -> m_pBlocked  %~ (\x -> x-1) $ m
                        (Just Runnable) -> m_pRunnable %~ (\x -> x-1) $ m
                        Nothing         -> m_pTotal    %~ (\x -> x+1) $ m
                      m'' = case newState of
                       --increment the new state counter, or remove the event
                       --from the total
                        (Just Running)  -> m_pRunning  %~ (\x -> x+1) $ m'
                        (Just Blocked)  -> m_pBlocked  %~ (\x -> x+1) $ m'
                        (Just Runnable) -> m_pRunnable %~ (\x -> x+1) $ m'
                        Nothing         -> m_pTotal    %~ (\x -> x-1) $ m'
                  in m''

{-
 - sets the Machine State according to the current Process count.
 - -}
setMachineState :: MachineState -> MachineState
    -- no Processess Assigned, this Machine is idle
setMachineState m
                    | _m_pTotal m == 0             = m {_m_state = Idle}
    --if a single process is running, this Machine will be running.
                    | _m_pRunning m >0             = m {_m_state = Running}
    --if all Processes are blocked, this Machine is blocked.
                    | _m_pBlocked m == _m_pTotal m = m {_m_state = Blocked}
    --if at least one Process is Runnable, this Machine is runnable.
                    | _m_pRunnable m >0             = m {_m_state = Runnable}

{-
 - takes a state transition within a thread and the parent process,
 - and updates it accordingly.
 - -}
updateThreadCountAndProcessState :: MachineId
                                    -> ProcessId
                                    -> Timestamp
                                    -> ProcessState
                                    -> (Maybe RunState)
                                    -> (Maybe RunState)
                                    -> (ProcessState, Maybe GUIEvent)
updateThreadCountAndProcessState mid pid ts p oldState newState  = let
    p'  = updateThreadCount p oldState newState
    p'' = setProcessState p'
    in if p^.p_state == p''^.p_state
        then (p'',Nothing)
        else (set p_state (p''^.p_state) $ set p_timestamp ts $ p'', Just $ GUIEvent {
            mtpType   = Process mid pid,
            startTime = p^.p_timestamp,
            duration  = ts - p^.p_timestamp,
            state     = p^.p_state
            })

{-
 - updates the inner Thread count of a Process. If the Thread was newly
 - created, oldState is Nothing. If the Thread is being killed, newState
 - is Nothing.
 - -}
updateThreadCount :: ProcessState ->  (Maybe RunState) -> (Maybe RunState)
                                  ->  ProcessState
updateThreadCount p oldState newState
    | oldState == newState = p
    --for testing, threads cannot be idle.
    | (oldState == Just Idle) || (newState == Just Idle) = p
    | otherwise = let p' = case oldState of
                        --decrement the old state counter, or insert the event.
                        (Just Running)  -> p_tRunning  %~ (\x -> x-1) $ p
                        (Just Blocked)  -> p_tBlocked  %~ (\x -> x-1) $ p
                        (Just Runnable) -> p_tRunnable %~ (\x -> x-1) $ p
                        Nothing         -> p_tTotal    %~ (\x -> x+1) $ p
                      p'' = case newState of
                       --increment the new state counter, or remove the event
                       --from the total
                        (Just Running)  -> p_tRunning  %~ (\x -> x+1) $ p'
                        (Just Blocked)  -> p_tBlocked  %~ (\x -> x+1) $ p'
                        (Just Runnable) -> p_tRunnable %~ (\x -> x+1) $ p'
                        Nothing         -> p_tTotal    %~ (\x -> x-1) $ p'
                  in p''

-- A thread event will adjust the counters of a process event.
-- this function will then adjust the internal state.
setProcessState :: ProcessState -> ProcessState
    -- no Threads Assigned, this process is idle.
setProcessState p   | p^.p_tTotal == 0             = p {_p_state = Idle}
    --if a single thread is running, this process will be running.
                    | p^.p_tRunning >0             = p {_p_state = Running}
    --if all threads are blocked, this thread is blocked.
                    | p^.p_tBlocked == p^.p_tTotal = p {_p_state = Blocked}
    --if at least one Thread is Runnable, this process is runnable.
                    | p^.p_tRunnable >0             = p {_p_state = Runnable}

