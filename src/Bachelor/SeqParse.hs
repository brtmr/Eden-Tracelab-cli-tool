{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse (parse, ParserState) where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.Parsers
import qualified Bachelor.DataBase as DB
import qualified Bachelor.TinyZipper as TZ
import qualified Database.PostgreSQL.Simple as PG
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
    _p_pt       :: ParserTable,   -- event types and their parsers
    _p_cap      :: Int,           -- the capability we are interested in
    _p_con      :: PG.Connection
        }

$(makeLenses ''ParserState)

-- instead of parsing a single *.eventlog file, we want to parse a *.parevents
-- file, which is a zipfile containing a set of *.eventlog files, one for
-- every Machine used.
run :: FilePath -> IO()
run fn = do
    c <- TZ.readZip fn
    case c of
        Left err -> error $ "could not open the *.parseEvents File : " ++ err
        Right bss -> do
            print $ length bss

-- | parses a single *.eventlog file for the events of the capability n
parse :: FilePath -- ^ Path to the *.Eventlog file.
    -> Int --the capability
    -> IO ()
parse file
    n = do
    --open the DataBase Connection
    con <- DB.mkConnection
    bs <- LB.readFile file
    -- read the Header
    case (AL.parse headerParser bs) of
        AL.Fail{}               -> error "Header Parsing failed."
        (AL.Done bsrest header) -> do
            let pt     = mkParserTable header
                bsdata = LB.drop 4 bsrest
            let state = ParserState {
                _p_bs       = bsdata,
                _p_rtsState = startingState,
                _p_cap      = n,
                _p_con      = con,
                _p_pt       = pt
                }
            handleEvents state

handleEvents :: ParserState -> IO()
handleEvents ps = do
    let s = AL.parse (parseSingleEvent (ps^.p_pt)) (ps^.p_bs)
    case s of
        AL.Fail{}                 -> error "Failed parsing Events "
        (AL.Done bsrest (Just e))  -> do
            putStrLn $ show e
            handleEvents (ps {_p_bs = bsrest})
        (AL.Done bsrest Nothing)  -> do
            putStrLn $ show $ "Done."
{-
machineLens m_id = p_rtsState._1.(at m_id)
processLens p_id = p_rtsState._2.(at p_id)
threadLens  t_id = p_rtsState._3.(at t_id)
-}

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


