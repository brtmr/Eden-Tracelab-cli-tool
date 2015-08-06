{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Bachelor.Parsers where

import GHC.RTS.Events
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as Bin
import qualified Data.HashMap.Strict as M
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Applicative
import Data.Bits
import System.Environment

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
-- needed for (# unboxing #) with magic hash
import GHC.Base
import GHC.Word
#endif

-- copy/pasted from GHC.RTS.EventTypes, because these are not exported.
type EventTypeNum     = Word16
type EventTypeDescLen = Word32
type EventTypeDesc    = String
type EventTypeSize    = Word16

type Capset = Word32
type TaskId = Word64
type ProcessId = Word32
type MachineId = Word16
type PortId = ThreadId
type MessageSize = Word32
type RawMsgTag = Word8


type SizeTable = M.HashMap EventTypeNum (Maybe EventTypeSize)

main :: IO()
main = do
    args <- getArgs
    let fn = head args
    run fn

run :: String -> IO()
run fn = do
    bs <- B.readFile fn
    case A.parse eventLogParser bs of
        (A.Fail i sl s) -> putStrLn $ "parse failed " ++ show sl ++ " " ++ s ++ (show (B.take 100 i))
        A.Partial{}     -> putStrLn "partial"
        (A.Done i (EventLog _ (Data a)))    -> putStrLn $ "Done. " ++ (show $ length a)

{-
instance Eq Event where
    (==) (Event t1 (EventBlock _ _ _)) (Event t2 (EventBlock _ _ _)) = t1==t2
    (==) (Event t1 spec1) (Event t2 spec2) = t1 == t2 && spec1 == spec2
-}

eventLogParser :: A.Parser EventLog
eventLogParser = do
    eventLogHeader <- headerParser
    let sizeTable = mkSizeTable eventLogHeader
    events <- parseEventStream sizeTable
    return $ EventLog eventLogHeader (Data events)

mkSizeTable :: Header -> SizeTable
mkSizeTable h = foldr (\e m -> M.insert (num e) (size e) m) M.empty (eventTypes h)

headerParser :: A.Parser Header
headerParser = do
    _ <- A.string $ C.pack "hdrb" -- begin header
    _ <- A.string $ C.pack "hetb" -- begin event type list
    typeList <- A.many1' eventTypeParser
    _ <- A.string $ C.pack "hete" -- end header
    _ <- A.string $ C.pack "hdre" -- end event type list
    return $ Header typeList

eventTypeParser :: A.Parser EventType
eventTypeParser = do
    _       <- A.string $ C.pack "etb\NUL" --begin event type
    id_    <- word16be <$> A.take 2
    eventTypeSize   <- word16be <$> A.take 2
    sizeName <- word32be <$> A.take 4
    name   <- A.take (fromIntegral sizeName)
    sizeExtraInfo <- fromIntegral <$> word32be <$> A.take 4
    _ <- A.take sizeExtraInfo
    _       <- A.string $ C.pack "ete\NUL" --end event type
    let v = eventTypeSize == 0xFFFF
    return $ EventType id_ (C.unpack name) (if v then Nothing else Just eventTypeSize)

parseEventStream :: SizeTable -> A.Parser [Event]
parseEventStream st = do
    _ <- A.string $ C.pack "datb"
    events <- A.many1 (parseEvent st)
    events <- parseEvents st
    _ <- Bin.word16be 0xFFFF --parse Events until the
    return events

parseEvents :: SizeTable -> A.Parser [Event]
parseEvents st = do
    event <- parseEvent st
    case event of
        Nothing -> return []
        (Just e)  -> do
            restEvents <- parseEvents st
            return $ e : restEvents

-- parses relevant Events, skips otherwise
-- This parser is not compatible with eventlogs created wtype Capset   = Word32
parseEvent :: SizeTable -> A.Parser (Maybe Event)
parseEvent st = do
    type_     <- word16be <$> A.take 2 -- the type (spec) of the event
    if (type_==0xFFFF)
        then return Nothing
        else do
            timestamp <- word64be <$> A.take 8 -- the timestamp
            let maybeSize = st M.! type_ --lookup the size of the event in the table
            case maybeSize of
                (Just s) ->
                    case type_ of
                        0 -> do -- CreateThread
                                threadId <- word32be <$> A.take 4
                                return $ Just $ Event timestamp (CreateThread threadId)
                        1 -> do -- RunThread
                                threadId <- word32be <$> A.take 4
                                return $ Just $ Event timestamp (RunThread threadId)
                        2 -> do -- StopThread
                                threadId <- word32be <$> A.take 4
                                blockreason <- word16be <$> A.take 2
                                _ <- word32be <$> A.take 4
                                    -- used in older ghcs by BlockedOnBlackHoleOwnedBy
                                    -- ignored here.
                                return $ Just $ Event timestamp (StopThread threadId
                                        (mkStopStatus blockreason))
                        3 -> do -- ThreadRunnable
                                threadId <- word32be <$> A.take 4
                                return $ Just $ Event timestamp (ThreadRunnable threadId)
                        4 -> do -- MigrateThread
                                threadId <- word32be <$> A.take 4
                                capId    <- fromIntegral <$> word16be <$> A.take 2
                                return $ Just $ Event timestamp (MigrateThread threadId capId)
                        --5-7 deprecated
                        8 -> do --WakeupThread
                                threadId    <- word32be <$> A.take 4
                                otherCap    <- word16be <$> A.take 2
                                return $ Just $ Event timestamp (MigrateThread threadId
                                    (fromIntegral otherCap))
                        9 -> return $ Just $ Event timestamp StartGC
                        10-> return $ Just $ Event timestamp EndGC
                        11-> return $ Just $ Event timestamp RequestSeqGC
                        12-> return $ Just $ Event timestamp RequestParGC
                        --13/14 deprecated
                        15-> do
                            threadId <-word32be <$> A.take 4
                            return $ Just $ Event timestamp (CreateSparkThread threadId)
                        --16 variable sized
                        17-> do
                            cap <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapCreate cap)
                        18->do
                            blockSize <- word32be <$> A.take 4
                            endTime   <- word64be <$> A.take 8
                            cap       <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (EventBlock timestamp cap [])
                        20-> return $ Just $ Event timestamp GCIdle
                        21-> return $ Just $ Event timestamp GCWork
                        22-> return $ Just $ Event timestamp GCDone
                        25-> do
                            capSet <- word32be <$> A.take 4
                            capSetTypeId <-word16be <$> A.take 2
                            let capSetType = mkCapsetType capSetTypeId
                            return $ Just $ Event timestamp (CapsetCreate capSet capSetType)
                        26-> do
                            capSet <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (CapsetDelete capSet)
                        27-> do
                            capSet <- word32be <$> A.take 4
                            cap    <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapsetAssignCap capSet cap)
                        28 -> do
                            capSet <- word32be <$> A.take 4
                            cap    <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapsetRemoveCap capSet cap)
                        32 -> do
                            capSet <- word32be <$> A.take 4
                            pid    <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (OsProcessPid capSet pid)
                        33 -> do
                            capSet <- word32be <$> A.take 4
                            pid    <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (OsProcessParentPid capSet pid)
                        34 -> do
                            crt <- word64be <$> A.take 8
                            dud <- word64be <$> A.take 8
                            ovf <- word64be <$> A.take 8
                            cnv <- word64be <$> A.take 8
                            gcd <- word64be <$> A.take 8
                            fiz <- word64be <$> A.take 8
                            rem <- word64be <$> A.take 8
                            return $ Just $ Event timestamp SparkCounters{sparksCreated    = crt, sparksDud       = dud,
                                                 sparksOverflowed = ovf, sparksConverted = cnv,
                                                 sparksFizzled    = fiz, sparksGCd       = gcd,
                                                 sparksRemaining  = rem}
                        35-> return $ Just $ Event timestamp SparkCreate
                        36-> return $ Just $ Event timestamp SparkDud
                        37-> return $ Just $ Event timestamp SparkOverflow
                        38-> return $ Just $ Event timestamp SparkRun
                        39-> do
                            vic <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (SparkSteal vic)
                        40-> return $ Just $ Event timestamp SparkFizzle
                        41-> return $ Just $ Event timestamp SparkGC
                        43-> do
                            capSet <- word32be <$> A.take 4
                            unixEpoch <- word64be <$> A.take 8
                            nanoseconds <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (WallClockTime capSet unixEpoch nanoseconds)
                        45 -> do
                            cap <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapCreate cap)
                        46 -> do
                            cap <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapDelete cap)
                        47 -> do
                            cap <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapDisable cap)
                        48 -> do
                            cap <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (CapEnable cap)
                        49 -> do
                            cap <- word32be <$> A.take 4
                            bytes <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (HeapAllocated cap bytes)
                        50 -> do
                            cap <- word32be <$> A.take 4
                            bytes <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (HeapSize cap bytes)
                        51 -> do
                            cap <- word32be <$> A.take 4
                            bytes <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (HeapLive cap bytes)
                        52 -> do
                            cap <- word32be <$> A.take 4
                            gens <- fromIntegral <$> word16be <$> A.take 2
                            maxHeapSize   <- word64be <$> A.take 8
                            allocAreaSize <- word64be <$> A.take 8
                            mblockSize    <- word64be <$> A.take 8
                            blockSize     <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (HeapInfoGHC cap gens maxHeapSize
                                allocAreaSize mblockSize blockSize)
                        53 -> do
                            heapCapset   <- word32be <$> A.take 4
                            gen          <- fromIntegral <$> word16be <$> A.take 2
                            copied       <- word64be <$> A.take 8
                            slop         <- word64be <$> A.take 8
                            frag         <- word64be <$> A.take 8
                            parNThreads  <- fromIntegral <$> word32be <$> A.take 4
                            parMaxCopied <- word64be <$> A.take 8
                            parTotCopied <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (GCStatsGHC heapCapset gen
                                copied slop frag parNThreads parMaxCopied parTotCopied)
                        54 -> return $ Just $ Event timestamp GlobalSyncGC
                        55 -> do
                            taskId <- word64be <$> A.take 8
                            cap    <- fromIntegral <$> word16be <$> A.take 2
                            tid    <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (TaskCreate taskId cap (KernelThreadId tid))
                        56 -> do
                            taskId <- word64be <$> A.take 8
                            cap    <- fromIntegral <$> word16be <$> A.take 2
                            capNew <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (TaskMigrate taskId cap capNew)
                        57 -> do
                            taskId <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (TaskDelete taskId)
                        60 -> return $ Just $ Event timestamp EdenStartReceive
                        61 -> return $ Just $ Event timestamp EdenEndReceive
                        62 -> do
                            pid <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (CreateProcess pid)
                        63 -> do
                            pid <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (KillProcess pid)
                        64 -> do
                            tid <- word32be <$> A.take 4
                            pid <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (AssignThreadToProcess tid pid)
                        65 -> do
                            mid <- fromIntegral <$> word16be <$> A.take 2
                            realtime <- word64be <$> A.take 8
                            return $ Just $ Event timestamp (CreateMachine mid realtime)
                        66 -> do
                            mid <- fromIntegral <$> word16be <$> A.take 2
                            return $ Just $ Event timestamp (KillMachine mid)
                        67 -> do
                            tag <- A.anyWord8
                            sP  <- word32be <$> A.take 4
                            sT  <- word32be <$> A.take 4
                            rM  <- word16be <$> A.take 2
                            rP  <- word32be <$> A.take 4
                            rIP <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (SendMessage { mesTag = toMsgTag tag,
                                                 senderProcess = sP,
                                                 senderThread = sT,
                                                 receiverMachine = rM,
                                                 receiverProcess = rP,
                                                 receiverInport = rIP
                                               })
                        68 -> do
                            tag <- A.anyWord8
                            rP  <- word32be <$> A.take 4
                            rIP <- word32be <$> A.take 4
                            sM  <- word16be <$> A.take 2
                            sP  <- word32be <$> A.take 4
                            sT  <- word32be <$> A.take 4
                            mS  <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (ReceiveMessage { mesTag = toMsgTag tag,
                                                     receiverProcess = rP,
                                                     receiverInport = rIP,
                                                     senderMachine = sM,
                                                     senderProcess = sP,
                                                     senderThread= sT,
                                                     messageSize = mS
                                                   })
                        69 -> do
                            tag <- A.anyWord8
                            sP  <- Bin.anyWord32le
                            sT  <- Bin.anyWord32le
                            rP  <- Bin.anyWord32le
                            rIP <- Bin.anyWord32le
                            return $ Just $ Event timestamp (SendReceiveLocalMessage { mesTag = toMsgTag tag,
                                                             senderProcess = sP,
                                                             senderThread = sT,
                                                             receiverProcess = rP,
                                                             receiverInport = rIP
                                                           })
                        _ -> do --for all event types not yet implemented.
                                _ <- A.take (fromIntegral s)
                                return $ Just $ Event timestamp (UnknownEvent type_)
                Nothing -> do
                    case type_ of
                        16 -> do
                            varDataLength <- word16be <$> A.take 2
                            varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                            return $ Just $ Event timestamp (Message varData)
                        19 -> do
                            varDataLength <- word16be <$> A.take 2
                            varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                            return $ Just $ Event timestamp (UserMessage varData)
                        23 -> do
                            varDataLength <- word16be <$> A.take 2
                            varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                            return $ Just $ Event timestamp (Version varData)
                        24 -> do
                            varDataLength <- word16be <$> A.take 2-- Warning: order of fiz and gcd reversed!ord16be
                            varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                            return $ Just $ Event timestamp (Version varData)
                        29 -> do
                            varDataLength <- fromIntegral <$> word16be <$> A.take 2
                            capSet <- word32be <$> A.take 4
                            varData <- C.unpack <$> A.take (varDataLength - 4)
                            return $ Just $ Event timestamp (RtsIdentifier capSet varData)
                        30 -> do
                            varDataLength <- fromIntegral <$> word16be <$> A.take 2
                            capSet <- word32be <$> A.take 4
                            varData <- C.unpack <$> A.take (varDataLength - 4)
                            return $ Just $ Event timestamp (ProgramArgs capSet (splitNull varData))
                        31 -> do
                            varDataLength <- fromIntegral <$> word16be <$> A.take 2
                            capSet <- word32be <$> A.take 4
                            varData <- C.unpack <$> A.take (varDataLength - 4)
                            return $ Just $ Event timestamp (ProgramEnv capSet (splitNull varData))
                        42 -> do
                            varDataLength <- fromIntegral <$> word16be <$> A.take 2
                            string <- C.unpack <$> A.take (varDataLength - 4)
                            stringId <- word32be <$> A.take 4
                            return $ Just $ Event timestamp (InternString string stringId)
                        44 -> do
                            varDataLength <- fromIntegral <$> word16be <$> A.take 2-- Warning: order of fiz and gcd reversed!ord16be
                            threadId <- word32be <$> A.take 4
                            varData <- C.unpack <$> A.take (varDataLength - 4)
                            return $ Just $ Event timestamp (ThreadLabel threadId varData)
                        58 -> do
                            varDataLength <- word16be <$> A.take 2-- Warning: order of fiz and gcd reversed!ord16be
                            varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                            return $ Just $ Event timestamp (UserMarker varData)

                        _ -> do
                            eventSize  <- word16be <$> A.take 2
                            _  <- A.take $ fromIntegral eventSize
                            return $ Just $ Event timestamp (UnknownEvent type_)

{-blatantly copied from GHC.RTS.Eventypes: -}
type RawThreadStopStatus = Word16

mkStopStatus :: RawThreadStopStatus -> ThreadStopStatus
mkStopStatus n = case n of
    0  ->  NoStatus
    1  ->  HeapOverflow
    2  ->  StackOverflow
    3  ->  ThreadYielding
    4  ->  ThreadBlocked
    5  ->  ThreadFinished
    6  ->  ForeignCall
    7  ->  BlockedOnMVar
    8  ->  BlockedOnBlackHole
    9  ->  BlockedOnRead
    10 ->  BlockedOnWrite
    11 ->  BlockedOnDelay
    12 ->  BlockedOnSTM
    13 ->  BlockedOnDoProc
    14 ->  BlockedOnCCall
    15 ->  BlockedOnCCall_NoUnblockExc
    16 ->  BlockedOnMsgThrowTo
    17 ->  ThreadMigrating
    18 ->  BlockedOnMsgGlobalise
    19 ->  NoStatus -- yeuch... this one does not actually exist in GHC eventlogs
    20 ->  BlockedOnMVarRead -- sincRawThreadStopStatustatus -> ThreadStopStatus

mkCapsetType :: Word16 -> CapsetType
mkCapsetType n = case n of
 1 -> CapsetCustom
 2 -> CapsetOsProcess
 3 -> CapsetClockDomain
 _ -> CapsetUnknown

splitNull :: String -> [String]
splitNull [] = []
splitNull xs = case span (/= '\0') xs of
                (x, xs') -> x : splitNull (drop 1 xs')

offset = 0x50
toMsgTag :: RawMsgTag -> MessageTag
toMsgTag = toEnum . fromIntegral . (\n -> n - offset)

{- transforming ByteStrings to unsingned integer types. -}
{- taken from Data.Binary.Get -}

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE word64be #-}

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE word32be #-}

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))

{-# INLINE word16be #-}

-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
