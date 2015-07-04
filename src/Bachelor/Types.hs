{-
 - A Set of types to describe Events that can be displayed within edenTv
 -}
{-# LANGUAGE TemplateHaskell #-}

module Bachelor.Types where

import Control.Lens
import Data.Word
-- The GUIEvent type describes events that can be displayed within the
-- edentv GUI. All of these have a specified starting time and duration
-- | describes the current state of the RTS at the current moment in time
type MachineID = Word16
type ProcessID = Word64
type ThreadID = Word64
type Time = Word64

data RunState = Idle | Running | Blocked | Runnable


data ProcessState = ProcessState {
    _threads :: [(ThreadID, Time, RunState)]
}
$(makeLenses ''ProcessState)
data MachineState = MachineState {
    _processes :: [(ProcessID, Time, RunState, ProcessState)]
}
$(makeLenses ''MachineState)

data RTSState = RTSState {
    _machines :: [(MachineID, Time, RunState, MachineState)]
}
$(makeLenses ''RTSState)

data GUIEvent = GUIEvent

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
