{-
 - A Set of types to describe Events that can be displayed within edenTv
 -}
{-# LANGUAGE TemplateHaskell #-}

module Bachelor.Types where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Word
import GHC.RTS.Events (ThreadId, MachineId, ProcessId, Timestamp)
-- The GUIEvent type describes events that can be displayed within the
-- edentv GUI. All of these have a specified starting time and duration
-- | describes the current state of the RTS at the current moment in time

type Time = Word64

type ThreadMap  = M.HashMap ThreadId  (RunState, Timestamp)
type ProcessMap = M.HashMap ProcessId (RunState, Timestamp, [ThreadId ])
type MachineMap = M.HashMap MachineId (RunState, Timestamp, [ProcessId])

data RunState = Idle | Running | Blocked | Runnable
    deriving (Show, Eq)
type RTSState = (MachineMap, ProcessMap, ThreadMap)

data MtpType = Machine MachineId | Process ProcessId | Thread ThreadId deriving Show

startingState :: RTSState
startingState = (M.empty, M.empty, M.empty)

{- auxiliary functions for manipulation RTSState -}

--returns a list of blocked Processes
--a Process is blocked when all its threads are blocked
processesBlocked :: RTSState -> [ProcessId]
processesBlocked rts@(_,processes,_) =
    map fst $ M.toList $ M.filter
        (\(_,_,threads) -> threadsInState rts Blocked threads) processes

--given a list of threads, returns wether they are in RunState state.
threadsInState :: RTSState -> RunState -> [ThreadId] -> Bool
threadsInState rts@(_,_,threadMap) state threads =
    all (threadInState rts state) threads

--given a list of threads, returns wether at least one of them is in RunState
--state.
oneThreadInState :: RTSState -> RunState -> [ThreadId] -> Bool
oneThreadInState rts@(_,_,threadMap) state threads =
    any (threadInState rts state) threads

threadInState :: RTSState -> RunState -> ThreadId -> Bool
threadInState rts@(_,_,threadMap) state tid = case threadMap M.! tid of
            (state,_) -> True
            _ -> False

-- okay, now we can make the adjustments, that a single change can make to
-- the RTSState.
-- when the state of a thread has been changed, check the processes need to
-- be changed.

--setProcessState :: RTSState


{-  -}

data GUIEvent = GUIEvent{
    mtpType   :: MtpType,
    startTime :: Word64,
    duration  :: Word64,
    state     :: RunState
    } | AssignTtoP ThreadId ProcessId | AssignPtoM ProcessId MachineId

--  | The Interface for reading/writing the data from disk.
class IOEventData a where
    readEvent  :: IOEventData a => a
        -> Integer -- ^ start time (in ns)
        -> Integer -- ^ end time (in ns)
        -> Integer -- ^ resolution (in ns). States smaller than this will not
                   -- be retreived from disk.
        -> IO [GUIEvent]
    -- | writes a single Event to Disk
    writeEvent :: IOEventData a => a -> GUIEvent -> IO()

