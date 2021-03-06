{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.Parsers
import Bachelor.Types
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List
import Data.Map.Lens
import Data.Maybe
import Debug.Trace
import GHC.RTS.Events hiding (machine)
import qualified Bachelor.DataBase as DB
import qualified Bachelor.Util as U
import qualified Data.Array.IArray as Array
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Directory as Dir
import qualified System.IO as IO


-- taken from RTSEventsParser.hs
instance Eq ThreadStopStatus where
    NoStatus       == NoStatus       = True
    HeapOverflow   == HeapOverflow   = True
    StackOverflow  == StackOverflow  = True
    ThreadYielding == ThreadYielding = True
    ThreadBlocked  == ThreadBlocked  = True
    ThreadFinished == ThreadFinished = True
    ForeignCall    == ForeignCall    = True
    BlockedOnMVar  == BlockedOnMVar  = True
    BlockedOnMVarRead  == BlockedOnMVarRead  = True
    -- since GHC-7.8.2/ghc-events-0.4.3.1
    BlockedOnBlackHole == BlockedOnBlackHole = True
    BlockedOnRead  == BlockedOnRead  = True
    BlockedOnWrite == BlockedOnWrite = True
    BlockedOnDelay == BlockedOnDelay = True
    BlockedOnSTM   == BlockedOnSTM   = True
    BlockedOnDoProc == BlockedOnDoProc = True
    BlockedOnCCall == BlockedOnCCall = True
    BlockedOnCCall_NoUnblockExc == BlockedOnCCall_NoUnblockExc = True
    BlockedOnMsgThrowTo == BlockedOnMsgThrowTo = True
    ThreadMigrating == ThreadMigrating = True
    BlockedOnMsgGlobalise == BlockedOnMsgGlobalise = True
    (BlockedOnBlackHoleOwnedBy _) == (BlockedOnBlackHoleOwnedBy _) = True
    _ == _  = False

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

type Handler = RTSState -> AssignedEvent -> (RTSState,[GUIEvent])
{-
 - This is the main function to handle events,
 - manipulate the rts state, and generate GUI Events.
 - -}
handleEvents :: Handler
handleEvents rts aEvent@(AssignedEvent event@(Event ts spec) mid cap) =
    case spec of
        KillProcess pid ->
                killProcess rts mid pid ts
        KillMachine mid ->
                killMachine rts mid ts
        CreateMachine mid realtime  ->
            let newMachine = MachineState {
                _m_state     = Idle,
                _m_timestamp = ts,
                _m_pRunning  = 0,
                _m_pRunnable = 0,
                _m_pBlocked  = 0,
                _m_pIdle     = 0,
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
                (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                    (rts^.rts_machine) Nothing (Just Runnable)
                rts' = set (rts_processes.(at pid)) (Just newProcess)
                    $  set rts_machine newMachine
                    $  rts
            in (rts', creationEvent : mList [mEvent])
        AssignThreadToProcess tid pid ->
            let newThread = ThreadState {
                _t_parent      = pid,
                _t_state       = Runnable,
                _t_timestamp   = ts
                }
                creationEvent  = Just $ NewThread mid pid tid
                oldProcess          = (rts^.rts_processes) M.! pid
                (newProcess,pEvent) = updateThreadCountAndProcessState
                    mid pid ts oldProcess Nothing (Just Runnable)
                oldProcessState     = oldProcess^.p_state
                newProcessState     = newProcess^.p_state
                (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                    (rts^.rts_machine) (Just oldProcessState)
                    (Just newProcessState)
                rts' = set rts_machine newMachine $
                       set (rts_threads.(at tid))   (Just newThread)   $
                       set (rts_processes.(at pid)) (Just newProcess) $ rts
            in (rts', mList [creationEvent, pEvent, mEvent])
        RunThread tid ->
            changeThreadState rts mid tid Running ts
        WakeupThread tid _->
            changeThreadState rts mid tid Running ts
        --the StopThread event has several stopreasons, some of which will only
        --suspend the thread, one of which will kill it.
        StopThread tid reason ->
            if reason == ThreadFinished
                then do
                    killThread rts mid tid ts
                    --changeThreadState rts mid tid Blocked ts
                else if reason `elem` blockList
                    then do
                        changeThreadState rts mid tid Blocked ts
                    else do
                        changeThreadState rts mid tid Runnable ts
        {-
         - not observed, because EdenTV also does not observe it.
        ThreadRunnable tid ->
            changeThreadState rts mid tid Runnable ts
            -}
        _ -> (rts,[])


--taken directly from RTSEventsParser.hs, the list of reasons to block
--a thread
blockList = [ThreadBlocked, BlockedOnMVar, BlockedOnBlackHole, BlockedOnDelay,
    BlockedOnSTM, BlockedOnDoProc, BlockedOnMsgThrowTo, ThreadMigrating,
    BlockedOnMsgGlobalise, BlockedOnBlackHoleOwnedBy 0]
{-
 - We often have to deal with lists of type [Maybe GUIEvent], and want to
 - filter the actual events.
 - -}
mList = (map fromJust).(filter isJust)

{-
 - generalized function for changing the state of a thread, and updating
 - the parent process and machine.
 -}
changeThreadState :: RTSState -> MachineId -> ThreadId -> RunState-> Timestamp
                     -> (RTSState, [GUIEvent])
changeThreadState rts mid tid state ts =
    if M.member tid (rts^.rts_threads)
        then let
            oldThread           = (rts^.rts_threads) M.! tid
            oldState            = oldThread^.t_state
            pid                 = oldThread^.t_parent
            oldProcess          = (rts^.rts_processes) M.! pid
            (newThread,tEvent)  = setThreadState mid tid oldThread ts
                state
            (newProcess,pEvent) = updateThreadCountAndProcessState
                mid pid ts oldProcess (Just oldState) (Just state)
            oldProcessState     = oldProcess^.p_state
            newProcessState     = newProcess^.p_state
            (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                (rts^.rts_machine) (Just oldProcessState)
                (Just newProcessState)
            rts' = set rts_machine newMachine $
                   set (rts_threads.(at tid))   (Just newThread)  $
                   set (rts_processes.(at pid)) (Just newProcess) $ rts
        in (rts', mList [tEvent, pEvent, mEvent])
    --ignore 'homeless' threads.
    else (rts,[])

killThread :: RTSState -> MachineId -> ThreadId -> Timestamp
                -> (RTSState, [GUIEvent])
killThread rts mid tid ts =
    if M.member tid (rts^.rts_threads)
        then let
            oldThread           = (rts^.rts_threads) M.! tid
            oldState            = oldThread^.t_state
            pid                 = oldThread^.t_parent
            oldProcess          = (rts^.rts_processes) M.! pid
            tEvent              = GUIEvent {
                mtpType   = Thread mid tid,
                startTime = oldThread^.t_timestamp,
                duration  = ts - oldThread^.t_timestamp,
                state     = oldState }
            (newProcess,pEvent) = updateThreadCountAndProcessState
                mid pid ts oldProcess (Just oldState) Nothing
            oldProcessState     = oldProcess^.p_state
            newProcessState     = newProcess^.p_state
            (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
                (rts^.rts_machine) (Just oldProcessState)
                (Just newProcessState)
            rts' = set rts_machine newMachine $
                   set (rts_threads.(at tid)) Nothing$
                   set (rts_processes.(at pid)) (Just newProcess) $ rts
        in  (rts', tEvent : mList [pEvent, mEvent])
    --ignore 'homeless' threads.
    else (rts,[])

killMachine :: RTSState -> MachineId -> Timestamp -> (RTSState, [GUIEvent])
killMachine rts mid ts = let
    pids     = map fst $ M.toList $ rts^.rts_processes
    ptEvents = concat $ map (snd.(\pid->killProcess rts mid pid ts)) pids
    m        = rts^.rts_machine
    mEvent   = GUIEvent {
            mtpType   = Machine mid,
            startTime = _m_timestamp m,
            duration  = ts - _m_timestamp m,
            state     = _m_state m
        }
    in (RTSState PreMachine M.empty M.empty, mEvent:ptEvents)

killProcess :: RTSState -> MachineId -> ProcessId -> Timestamp -> (RTSState, [GUIEvent])
killProcess rts mid pid ts = let
        endThread :: (ThreadId,ThreadState) -> GUIEvent
        endThread (tid,t) = GUIEvent {
                mtpType   = Thread mid tid,
                startTime = t^.t_timestamp,
                duration  = ts - t^.t_timestamp,
                state     = t^.t_state }
        tEvents = map endThread
            $ filter (\(tid,t) -> t^.t_parent == pid)
            $ M.toList (rts^.rts_threads)
        rts'    = over rts_threads (M.filter (\x -> x^.t_parent/=pid)) $ rts
        rts''   = set (rts_processes.(at pid)) Nothing $ rts'
        p       = (rts^.rts_processes) M.! pid
        pEvent  = Just $ GUIEvent {
            mtpType   = Process mid pid,
            startTime = p^.p_timestamp,
            duration  = ts - p^.p_timestamp,
            state     = p^.p_state
            }
        (newMachine,mEvent) = updateProcessCountAndMachineState mid ts
            (rts^.rts_machine) (Just (p^.p_state)) Nothing
        rts''' = set rts_machine newMachine $ rts''
    in (rts''', mList [pEvent,mEvent] ++ tEvents)


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
    | otherwise = let m' = case oldState of
                        --decrement the old state counter, or insert the event.
                        (Just Idle)     -> m_pIdle     %~ decr $ m
                        (Just Running)  -> m_pRunning  %~ decr $ m
                        (Just Blocked)  -> m_pBlocked  %~ decr $ m
                        (Just Runnable) -> m_pRunnable %~ decr $ m
                        Nothing         -> m_pTotal    %~ incr $ m
                      m'' = case newState of
                       --increment the new state counter, or remove the event
                       --from the total
                        (Just Idle)     -> m_pIdle     %~ incr $ m'
                        (Just Running)  -> m_pRunning  %~ incr $ m'
                        (Just Blocked)  -> m_pBlocked  %~ incr $ m'
                        (Just Runnable) -> m_pRunnable %~ incr $ m'
                        Nothing         -> m_pTotal    %~ decr $ m'
                  in m''

{-
 - sets the Machine State according to the current Process count.
 - -}
setMachineState :: MachineState -> MachineState
setMachineState m
    -- no Processess Assigned, this Machine is idle
                    | _m_pTotal m == 0             = m {_m_state = Idle}
    -- all Processes Idle, this machine is idle
                    | _m_pTotal m == _m_pIdle m    = m {_m_state = Idle}
    --if a single process is running, this Machine will be running.
                    | _m_pRunning m >0             = m {_m_state = Running}
    --if all Processes are blocked, this Machine is blocked.
                    | _m_pBlocked m == _m_pTotal m = m {_m_state = Blocked}
    --if at least one Process is Runnable, this Machine is runnable.
                    | _m_pRunnable m >0             = m {_m_state = Runnable}
    --there are blocked and idle processes. mark the machine as blocked.
                    | _m_pBlocked m >0 && _m_pIdle m >0 = m {_m_state = Blocked}
                    | otherwise = error $ "could not determine machine state" ++ show m

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

incr x = x + 1
decr x = if (x - 1) >= 0 then x-1 else error "Negative count"
updateThreadCount :: ProcessState ->  (Maybe RunState) -> (Maybe RunState)
                                  ->  ProcessState
updateThreadCount p oldState newState
    | oldState == newState = p
    --for testing, threads cannot be idle.
    | (oldState == Just Idle) || (newState == Just Idle) = p
    | otherwise = let p' = case oldState of
                        --decrement the old state counter, or insert the event.
                        (Just Running)  -> p_tRunning  %~ decr $ p
                        (Just Blocked)  -> p_tBlocked  %~ decr $ p
                        (Just Runnable) -> p_tRunnable %~ decr $ p
                        Nothing         -> p_tTotal    %~ incr $ p
                      p'' = case newState of
                       --increment the new state counter, or remove the event
                       --from the total
                        (Just Running)  -> p_tRunning  %~ incr $ p'
                        (Just Blocked)  -> p_tBlocked  %~ incr $ p'
                        (Just Runnable) -> p_tRunnable %~ incr $ p'
                        Nothing         -> p_tTotal    %~ decr $ p'
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
    -- create a parserState for each eventLog file.
    pStates <- zip mids <$> mapM createParserState files
    -- create the multistate that encompasses all machines.
    --let mState = MultiParserState {
    --    _machineTable = M.fromList pStates,
    --    _con = dbi}
    -- for testing purposes only: test the first machine
    --let m1 = fromJust $ (mState^.machineTable^.(at 2))
    conn <- DB.mkConnection
    PG.withTransaction conn $ do
        dbi <- DB.insertTrace conn dir
        dbi <- foldM handleMachine dbi pStates
        dbi <- DB.finalize dbi
        return ()

handleMachine :: DB.DBInfo -> (MachineId, ParserState) -> IO DB.DBInfo
handleMachine dbi (mid,pstate) = do
    print $ "Processing Machine no ." ++ (show mid)
    dbi <- parseSingleEventLog dbi mid pstate
    return dbi

{-
 - This is the main function for parsing a single event log and storing
 - the events contained within into the database.
 - -}
parseSingleEventLog :: DB.DBInfo -> MachineId -> ParserState -> IO DB.DBInfo
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
    (bss,evs@(Just es@(Event tss specs)))
    (bs0,ev0@(Just e0@(Event ts0 spec0)))
    rts pt) = if (tss < ts0)
        then do
            let aEvent = AssignedEvent es mid (-1)
                (newRTS, guiEvents) = handleEvents (pstate^.p_rtsState) aEvent
                pstate' = set p_rtsState newRTS $ pstate
            dbi <- foldM DB.insertEvent dbi guiEvents
            parseSingleEventLog dbi mid $ parseNextEventSystem pstate'
        else do
            let aEvent = AssignedEvent e0 mid 0
                (newRTS, guiEvents) = handleEvents (pstate^.p_rtsState) aEvent
                pstate' = set p_rtsState newRTS $ pstate
            dbi <- foldM DB.insertEvent dbi guiEvents
            parseSingleEventLog dbi mid $ parseNextEventNull pstate'

-- no more system events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@(Just e0@(Event ts0 spec0)))
    rts pt) = do
            let aEvent = AssignedEvent e0 mid 0
                (newRTS, guiEvents) = handleEvents (pstate^.p_rtsState) aEvent
                pstate' = set p_rtsState newRTS $ pstate
            dbi <- foldM DB.insertEvent dbi guiEvents
            parseSingleEventLog dbi mid $ parseNextEventNull pstate'
-- no more cap0 events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just es@(Event tss specs)))
    (bs0,ev0@Nothing)
    rts pt) = do
            let aEvent = AssignedEvent es mid (-1)
                (newRTS, guiEvents) = handleEvents (pstate^.p_rtsState) aEvent
                pstate' = set p_rtsState newRTS $ pstate
            dbi <- foldM DB.insertEvent dbi guiEvents
            parseSingleEventLog dbi mid $ parseNextEventSystem pstate'
-- no more events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@Nothing)
    rts pt) = return dbi
