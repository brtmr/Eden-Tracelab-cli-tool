{-# LANGUAGE CPP #-}
module Bachelor.SeqParse where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Data.Binary.Get
import Data.ByteString.Lazy
import GHC.RTS.Events

-- for parsers
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Bachelor.SeqParse.PreParse

data ParserState = ParserState {
    bs           :: ByteString,
    bsOffset     :: ByteOffset,
    rtsState      :: RTSState
    }

-- | describes the current state of the RTS at the current moment in time
data RTSState = RTSState

{- | consumes a lazy bytestring and returns a new ParserState that
 - has already consumed the header and contains the correct EventParsers
 -
 - the Header parsing is taken from the getEventLog-function
 - from ghc-events-parallel
| -}

getParsers :: ByteString -> IO ParserState
getParsers bs = undefined

getParsers' :: Get EventParsers
getParsers' = do
    header <- getHeader
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
        -- This test is complete, no-one has extended this event yet and all future
        -- extensions will use newly allocated event IDs.
        is_ghc_6 = Just sz_old_tid == do create_et <- M.lookup EVENT_CREATE_THREAD imap
                                         size create_et
        {-
        -- GHC6 writes an invalid header, we handle it here by using a
        -- different set of event parsers.  Note that the ghc7 event parsers
        -- are standard events, and can be used by other runtime systems that
        -- make use of threadscope.
        -}

        -- GHC-7.8.2 uses a different thread block status encoding,
        -- and therefore requires a different parser for the stop
        -- event. Later, in GHC-7.8.3, the old encoding was restored.
        -- GHC-7.8.2 can be recognised by presence and absence of
        -- events in the header:
        --   * User markers were added in GHC-7.8
        --   * an empty event HACK_BUG_T9003 was added in GHC-7.8.3
        -- This fix breaks software which uses ghc-events and combines
        -- user markers with the older stop status encoding. We don't
        -- know of any such software, though.
        is_pre77  = M.notMember EVENT_USER_MARKER imap
        is_ghc782 = M.member EVENT_USER_MARKER imap &&
                    M.notMember EVENT_HACK_BUG_T9003 imap

        stopParsers = if is_pre77 then pre77StopParsers
                      else if is_ghc782 then [ghc782StopParser]
                           else [post782StopParser]

        event_parsers = if is_ghc_6
                            then standardParsers ++ ghc6Parsers ++
                                parRTSParsers sz_old_tid
                            else standardParsers ++ ghc7Parsers
                                 ++ stopParsers
                                 ++ parRTSParsers sz_tid
                                 ++ mercuryParsers ++ perfParsers
        parsers = mkEventTypeParsers imap event_parsers
    return parsers
