{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Bachelor.SeqParse where

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Bachelor.SeqParse.PreParse
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Binary.Get
import Data.ByteString.Lazy as LB (readFile, ByteString)
import Data.IntMap (IntMap)
import GHC.Arr
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes
import GHC.RTS.Events
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM

data RTSState = RTSState
data ParserState = ParserState {
    _p_bs       :: LB.ByteString,
    _p_bsOffset :: ByteOffset,
    _p_parsers  :: EventParsers,
    _p_rtsState :: RTSState,
    _p_cap      :: Int,
    _p_skipMap  :: SkipMap
    }

$(makeLenses ''ParserState)

instance Show ParserState where
    show p = "Parser is " ++ (show $ p ^. p_bsOffset) ++ " bytes in."

-- | describes the current state of the RTS at the current moment in time

filename = "/home/basti/bachelor/traces/mergesort_large/mergesort#9.eventlog"

-- |
-- |
parse :: Int -> IO ()
parse n = do
    bs <- LB.readFile filename
    blockinfo <- findBlocks bs
    if (null (filter (\x->capNo x==n) blockinfo))
        then error $ "No EventBlocks for Capability "++(show n)++" found."
        else do
            let skipMap =  mkSkipMap n blockinfo
                (bs', offset, parsers) = getParsers bs
                pstate = ParserState bs' offset parsers RTSState n skipMap
                (datb,pstate') = runGetOrFailHard getWord32be pstate
            if (datb/=EVENT_DATA_BEGIN)
                then error "Data begin section not found"
                else do
                   -- putStrLn $ show blockinfo
                    putStrLn $ show skipMap
                    parseEvents pstate'

-- | Sequentially parse Events of Capability n
parseEvents :: ParserState -> IO()
parseEvents pstate =
    let p = pstate^.p_parsers
        getSingleEvent = runExceptT $ runReaderT (getEvent p) p
    in if (skipNeeded pstate)
        then do
            let pstate' = mkSkip pstate
            putStrLn $ "PreSkip  : " ++ (show $ pstate^.p_bsOffset)
            putStrLn $ "PastSkip : " ++ (show $ pstate'^.p_bsOffset)
            parseEvents pstate'
        else case (runGetOrFailHard getSingleEvent pstate) of
            ((Left err),_) -> error err
            ((Right Nothing),pstate'') -> return ()
            ((Right (Just e)),pstate'') -> do
                case (spec e) of
                    (EventBlock _ _ _) -> do
                        putStrLn $ show pstate''
                        error "EventBlock found!"
                    _                  -> do
                        --putStrLn $ show e
                        --putStrLn $ show pstate''
                        parseEvents pstate''

-- | If the parser is at the beginning of a blockevent, do the following:
-- | If the current event belongs to the Capability we are parsing for,
-- | the just skip the 'block' event (which should be 24 bytes long)
-- | otherwise skip the entire block

skipNeeded :: ParserState -> Bool
skipNeeded pstate =
    let cur     = (pstate^.p_bsOffset)
    in M.member cur (pstate^.p_skipMap)

mkSkip :: ParserState -> ParserState
mkSkip pstate = snd $ runGetOrFailHard
            (Data.Binary.Get.skip $ (pstate^.p_skipMap) M.! (pstate^.p_bsOffset))
            pstate

-- | executes runGetOrFail and updates the state of the parser
-- | produces an error in case the parsing failes.
runGetOrFailHard :: Get a -> ParserState -> (a, ParserState)
runGetOrFailHard g pstate = case (runGetOrFail g (_p_bs pstate)) of
        (Left  (bs',offset,err)) -> error err
        (Right (bs',offset,res)) -> (res,
            p_bs .~ bs' $ (p_bsOffset %~ (+offset) $ pstate)
            ) -- add the previous offsets and set the new bytestring

-- | consumes a lazy bytestring and returns a new ParserState that
-- | has already consumed the header and contains the correct EventParsers
getParsers :: ByteString -> (LB.ByteString, ByteOffset, EventParsers)
getParsers bs = case (runGetOrFail getParsers' bs) of
    (Left  (bs',offset,err)) -> error err
    (Right (bs',offset,res)) -> case res of
        (Left err) -> error err
        (Right par) ->  (bs', offset, EventParsers par)

{-
 - the Header parsing is taken from the getEventLog-function
 - from ghc-events-parallel
 -}
getParsers' :: Get (Either String (GHC.Arr.Array Int (GetEvents EventInfo)))
getParsers' = runExceptT $ do
    header <- getHeader
    let imap = IM.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
        -- This test is complete, no-one has extended this event yet and all future
        -- extensions will use newly allocated event IDs.
        is_ghc_6 = Just sz_old_tid == do create_et <- IM.lookup EVENT_CREATE_THREAD imap
                                         GHC.RTS.Events.size create_et
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
        is_pre77  = IM.notMember EVENT_USER_MARKER imap
        is_ghc782 = IM.member EVENT_USER_MARKER imap &&
                    IM.notMember EVENT_HACK_BUG_T9003 imap

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
