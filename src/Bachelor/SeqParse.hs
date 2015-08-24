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
import GHC.RTS.Events
import Data.Map.Lens
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
    }

$(makeLenses ''ParserState)
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
    pStates <- zip mids <$> mapM createParserState paths
    -- create the multistate that encompasses all machines.
    let mState = MultiParserState {
        _machineTable = M.fromList pStates,
        _con = dbi}
    print dbi

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
                bsdata = LB.take 4 bsrest --'datb'
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
        _                  -> error $ "failed parsing the first event for cap " ++ (show cap)

{-
 - This is the main function for parsing a single event log and storing
 - the events contained within into the database.
 - -}
parseSingleEventLog :: DB.DBInfo -> MachineId -> ParserState -> IO()
-- both capabilies still have events left. return the earlier one.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just (Event tss specs)))
    (bs0,ev0@(Just (Event ts0 spec0)))
    rts pt) = if (tss < ts0)
        then do
            print evs
            parseSingleEventLog dbi mid pstate {
                _p_caps = parseNextEvent (pstate^.p_caps) pt 0xFFFF
            }
        else do
            print ev0
            parseSingleEventLog dbi mid pstate {
                _p_caps = parseNextEvent (pstate^.p_caps) pt 0xFFFF
            }
-- no more system events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@(Just (Event ts0 spec0)))
    rts pt) = do
            print ev0
            parseSingleEventLog dbi mid pstate {
                _p_caps = parseNextEvent (pstate^.p_caps) pt 0xFFFF
            }
-- no more cap1 events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@(Just (Event tss specs)))
    (bs0,ev0@Nothing)
    rts pt) = do
            print evs
            parseSingleEventLog dbi mid pstate {
                _p_caps = parseNextEvent (pstate^.p_caps) pt 0xFFFF
            }
-- no more events.
parseSingleEventLog dbi mid pstate@(ParserState
    (bss,evs@Nothing)
    (bs0,ev0@Nothing)
    rts pt) = return ()

-- takes the parser state of a capability
-- replaces the event with the next one in the bytstring.
parseNextEvent :: CapState -> ParserTable -> Capability -> CapState
parseNextEvent (bs,_) pt cap =
    case AL.parse (parseSingleEvent pt cap) bs of
        AL.Done bsrest res -> (bsrest,res)
        _                  -> error $ "Failing to parse event "
                                        ++ (show $ LB.take 10 $ bs)
{-
    Handlers for the different EventTypes.
    Some do not create GUIEvents, so they just return the new ParserState
    Some do create GUIEvents, so they return (ParserState,[GUIEvent])
-}
-- the following events can directly produce an event, WITHOUT producing
-- additional events:

-- CreateMachine
-- CreateProcess

-- CreateThread
-- AssignThreadToProcess

-- Startup (not sure wether we need this at all.)
-- EdenStartReceive
-- EdenEndReceive
-- SendMessage
-- ReceiveMessage
-- SendReceiveLocalMessage

-- these events might generate additional events, so we have to check after
-- WakeupThread (might wake up the process & machine.)
-- StopThread   (might stop the process & machine.)
-- KillProcess  (kill all threads)
-- KillMachine  (kill all Processes)

{- when Events are handled, we need to know from which Eventlog they where
 - sourced, so they are annotated with additional Information:  -}

data AssignedEvent = AssignedEvent {
    event :: Event,
    machine :: MachineId,
    cap :: Int
    }

type HandlerType = MultiParserState -> AssignedEvent -> (MultiParserState,[GUIEvent])


