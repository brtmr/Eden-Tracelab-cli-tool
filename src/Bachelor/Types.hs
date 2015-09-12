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

data RunState = Idle | Running | Blocked | Runnable
    deriving (Show, Eq)

type Time = Word64

data ProcessState = ProcessState {
    _p_parent    :: MachineId,
    _p_state     :: RunState,
    _p_timestamp :: Timestamp,
    _p_tRunning  :: Int,
    _p_tRunnable :: Int,
    _p_tBlocked  :: Int,
    _p_tTotal    :: Int
    } deriving (Show, Eq)

$(makeLenses ''ProcessState)

data MachineState = MachineState {
    _m_state     :: RunState,
    _m_timestamp :: Timestamp,
    _m_pRunning  :: Int,
    _m_pRunnable :: Int,
    _m_pBlocked  :: Int,
    _m_pTotal    :: Int
    } | PreMachine deriving Show

$(makeLenses ''MachineState)

data ThreadState  = ThreadState {
    _t_parent      :: ProcessId,
    _t_state       :: RunState,
    _t_timestamp   :: Timestamp
    } deriving Show

$(makeLenses ''ThreadState)

type ThreadMap    = M.HashMap ThreadId ThreadState
type ProcessMap   = M.HashMap ProcessId ProcessState

stateToInt :: RunState -> Int
stateToInt Idle     = 0
stateToInt Running  = 1
stateToInt Blocked  = 2
stateToInt Runnable = 3

data RTSState = RTSState {
    _rts_machine  :: MachineState,
    _rts_processes :: ProcessMap,
    _rts_threads   :: ThreadMap
    } deriving Show

$(makeLenses ''RTSState)

-- creates an empty, idle Machine not containing any processes
makeRTSState :: MachineId -> RTSState
makeRTSState mid = RTSState {
    _rts_machine = PreMachine,
    _rts_processes = M.empty,
    _rts_threads   = M.empty
    }

data MtpType = Machine MachineId
    | Process MachineId ProcessId
    | Thread  MachineId ThreadId deriving (Eq, Show)

startingState :: RTSState
startingState = RTSState PreMachine M.empty M.empty

{- auxiliary functions for manipulation RTSState -}


{- Types for events that can be written to the database. -}
data GUIEvent = GUIEvent{
    mtpType   :: MtpType,
    startTime :: Word64,
    duration  :: Word64,
    state     :: RunState
    } | NewMachine MachineId | NewProcess MachineId ProcessId
      | NewThread MachineId ProcessId ThreadId deriving (Eq, Show)

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
