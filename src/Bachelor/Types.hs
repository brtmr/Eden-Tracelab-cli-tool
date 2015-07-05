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
type RTSState = (MachineMap, ProcessMap, ThreadMap)

data MtpType = Machine MachineId | Process ProcessId | Thread ThreadId

startingState :: RTSState
startingState = (M.empty, M.empty, M.empty)

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
