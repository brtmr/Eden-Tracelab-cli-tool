{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse (parse, ParserState) where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.Parsers
import qualified Bachelor.Util as U
import Bachelor.Types
import Control.Lens
import qualified Data.ByteString.Lazy as LB
import GHC.RTS.Events
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified System.IO as IO

data ParserState = ParserState {
    _p_bs       :: LB.ByteString, -- the file we are reading from
    _p_rtsState :: RTSState,      -- the inner state of the runtime
    _p_st       :: ParserTable,   -- event types and their parsers
    _p_cap      :: Int            -- the capability we are interested in
        }

$(makeLenses ''ParserState)

filename = "/home/basti/bachelor/traces/mergesort_large/mergesort#9.eventlog"
testRun = parse filename 1

-- | parses a single *.eventlog file for the events of the capability n
parse :: FilePath -- ^ Path to the *.Eventlog file.
    -> Int        -- ^ The capability to analyze
    -> IO ()
parse file
    n = do
    bs <- LB.readFile file
    -- read the Header
    case (AL.parse headerParser bs) of
        AL.Fail{}               -> error "Header Parsing failed."
        (AL.Done bsrest header) -> do
            let pt     = mkParserTable header
                bsdata = LB.drop 4 bsrest
            putStrLn $ show $ (LB.take 10 bsrest)
            putStrLn $ show $ (LB.take 10 bsdata)
            handleEvents 0 pt bsdata

handleEvents :: Int -> ParserTable -> LB.ByteString -> IO()
handleEvents counter pt bs = do
    let s = AL.parse (parseSingleEvent pt) bs
    case s of
        AL.Fail{}                 -> error ("Failed parsing Events "
            ++ show counter ++ show bs)
        (AL.Done bsrest (Just _))  -> do
            handleEvents (counter+1) pt bsrest
        (AL.Done bsrest Nothing)  -> do
            putStrLn $ show $ counter

machineLens m_id = p_rtsState._1.(at m_id)
processLens p_id = p_rtsState._2.(at p_id)
threadLens  t_id = p_rtsState._3.(at t_id)
{-
    Handlers for the different EventTypes.
    Some do not create GUIEvents, so they just return the new ParserState
    Some do create GUIEvents, so they return (ParserState,[GUIEvent])
-}

createMachineHandler :: ParserState -> MachineId -> Timestamp -> ParserState
createMachineHandler pstate m_id t =
    set (machineLens m_id) (Just (Idle,t,[])) pstate

createProcessHandler :: ParserState -> ProcessId -> Timestamp -> ParserState
createProcessHandler pstate p_id t =
    set (processLens p_id) (Just (Idle,t,[])) pstate

createThreadHandler :: ParserState -> ThreadId -> Timestamp -> ParserState
createThreadHandler pstate t_id t =
    set (threadLens t_id) (Just (Idle,t)) pstate

assignThreadToProcessHandler :: ParserState -> ThreadId -> ProcessId -> (ParserState,[GUIEvent])
assignThreadToProcessHandler pstate t_id p_id = (newstate, [assign])
    where newstate = over ((processLens p_id)._Just._3) ((:) t_id) pstate
          assign   = AssignTtoP t_id p_id

-- a Process has been killed.
-- kill the Process, and all its threads.
killProcessHandler :: ParserState -> ProcessId -> (ParserState,[GUIEvent])
killProcessHandler pstate p_id = undefined
