{-
 - A Set of types to describe Events that can be displayed within edenTv
 -}
{-# LANGUAGE TemplateHaskell #-}

module Bachelor.Types where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Word
-- The GUIEvent type describes events that can be displayed within the
-- edentv GUI. All of these have a specified starting time and duration
-- | describes the current state of the RTS at the current moment in time
type MachineID = Int
type ProcessID = Int
type ThreadID = Int
type Time = Word64

data RunState = Idle | Running | Blocked | Runnable
type RTSState = M.HashMap MachineID Mashine
type Mashine  = (Time,RunState,M.HashMap ProcessID Process)
type Process  = (Time,RunState,M.HashMap ThreadID Thread)
type Thread   = (Time,RunState)

startingState :: RTSState
startingState = M.empty

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
