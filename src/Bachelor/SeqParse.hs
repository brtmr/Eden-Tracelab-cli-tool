{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse (parse, ParserState) where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.Parsers
import Bachelor.Types
import Control.Applicative
import Control.Lens
import Data.List
import GHC.RTS.Events
import qualified Bachelor.DataBase as DB
import qualified Bachelor.TinyZipper as TZ
import qualified Bachelor.Util as U
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Directory as Dir
import qualified System.IO as IO

data ParserState = ParserState {
    _p_bs       :: LB.ByteString, -- the file we are reading from
    _p_rtsState :: RTSState,      -- the inner state of the runtime
    _p_pt       :: ParserTable,   -- event types and their parsers
    _p_cap      :: Int            -- the capability we are currently parsing.
        }

$(makeLenses ''ParserState)

-- paths:
testdir = "/home/basti/bachelor/traces/mergesort_large/"

-- instead of parsing a single *.eventlog file, we want to parse a *.parevents
-- file, which is a zipfile containing a set of *.eventlog files, one for
-- every Machine used.
-- because reading from a zipfile lazily seems somewhat troubling, we will
-- instead unzip all files beforehand and then read them all as single files.

run :: FilePath -> IO()
run dir = do
    paths <- filter (isSuffixOf ".eventlog") <$> Dir.getDirectoryContents dir
    dbi <- DB.createDBInfo dir
    print dbi
    mapM_ (\filename -> print $ extractNumbers filename) paths
    mapM_ (\filename -> parse $ dir ++ filename) paths

extractNumbers :: String -> Int
extractNumbers str = read $ reverse $ takeWhile (/= '#') $ drop 9 $ reverse str

-- | parses a single *.eventlog file for the events of the capability n
parse :: FilePath -- ^ Path to the *.Eventlog file.
    -> IO ()
parse file = do
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
                _p_cap      = -1,
                _p_pt       = pt
                }
            handleEvents state

handleEvents :: ParserState -> IO()
handleEvents ps = do
    let s = AL.parse (parseSingleEvent (ps^.p_pt)) (ps^.p_bs)
    case s of
        AL.Fail{}                 -> error "Failed parsing Events "
        (AL.Done bsrest (Just e))  -> do
            case e of
                Event _ CreateMachine{} -> do
                    putStrLn $ show e
                    handleEvents (ps {_p_bs = bsrest})
                _                       -> do
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


