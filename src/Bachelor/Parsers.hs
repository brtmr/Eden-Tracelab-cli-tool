{-# LANGUAGE CPP,BangPatterns,PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields -fwarn-incomplete-patterns #-}
module Bachelor.Parsers where

import Debug.Trace

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
import Bachelor.Util as U
import qualified Data.Array.IArray as IA

-- copy/pasted from GHC.RTS.EventTypes, because these are not exported.
type EventTypeNum     = Word16
type EventTypeDescLen = Word32
type EventTypeDesc    = String
type EventTypeSize    = Word16

type Capset = Word32
type TaskId = Word64
type PortId = ThreadId
type MessageSize = Word32
type RawMsgTag = Word8

type SizeTable = M.HashMap EventTypeNum (Maybe EventTypeSize)
type ParserTable = IA.Array Word16 (Word64 -> A.Parser Event)

run :: String -> IO()
run fn = do
    bs <- B.readFile fn
    case A.parse eventLogParser bs of
        (A.Fail i sl s) -> putStrLn $ "parse failed " ++ show sl ++ " " ++ s ++ (show (B.take 100 i))
        A.Partial{}     -> putStrLn "partial"
        (A.Done i (EventLog _ (Data a)))    -> putStrLn $ "Done. " ++ (show $ length a)

eventLogParser :: A.Parser EventLog
eventLogParser = do
    eventLogHeader <- headerParser
    let parserTable = mkParserTable eventLogHeader
    events <- parseEventStream parserTable
    return $ EventLog eventLogHeader (Data events)

mkSizeTable :: Header -> SizeTable
mkSizeTable h = foldr (\e m -> M.insert (num e) (size e) m) M.empty (eventTypes h)

mkParserTable :: Header -> ParserTable
mkParserTable h = IA.array (0,100) $ foldr addToList knownParsers (eventTypes h)
    where addToList :: EventType -> [(Word16, Word64 -> A.Parser Event)] -> [(Word16, Word64 -> A.Parser Event)]
          addToList (EventType id_ _ s) list = if (null $ filter (\x -> fst x == id_) list)
            then (id_ , makeUnknownParser id_ s) : (filter (\x -> fst x /= id_) list)
            else list

makeUnknownParser :: EventTypeNum -> Maybe EventTypeSize -> (Word64 -> A.Parser Event)
makeUnknownParser id_ (Just s) = (\timestamp -> do
    _ <- A.take (fromIntegral s)
    return $ Event timestamp (UnknownEvent id_))
makeUnknownParser id_ Nothing = (\timestamp -> do
    eventSize  <- U.parseW16
    _  <- A.take $ fromIntegral eventSize
    return $ Event timestamp (UnknownEvent id_))

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
    id_    <- U.parseW16
    eventTypeSize   <- U.parseW16
    sizeName <- U.parseW32
    name   <- A.take (fromIntegral sizeName)
    sizeExtraInfo <- fromIntegral <$> U.parseW32
    _ <- A.take sizeExtraInfo
    _       <- A.string $ C.pack "ete\NUL" --end event type
    let v = eventTypeSize == 0xFFFF
    return $ EventType id_ (C.unpack name) (if v then Nothing else Just eventTypeSize)

parseEventStream :: ParserTable -> A.Parser [Event]
parseEventStream st = do
    _ <- A.string $ C.pack "datb"
    events <- parseEvents st
    return events

parseEvents :: ParserTable -> A.Parser [Event]
parseEvents pt = do
    type_ <- U.parseW16
    if (type_ == 0xFFFF) then return []
        else do
            timestamp <- U.parseW64 -- the timestamp
            event <- (pt IA.! type_) timestamp
            rest <- parseEvents pt
            return $ event : rest

parseSingleEvent :: ParserTable -> A.Parser (Maybe Event)
parseSingleEvent pt = do
    type_ <- U.parseW16
    if (type_ == 0xFFFF)
        then return $ Nothing
        else do
            timestamp <- U.parseW64 -- the timestamp
            event     <- (pt IA.! type_) timestamp
            return $ Just event

knownParsers :: [(Word16, Word64 -> A.Parser Event)]
knownParsers = [ (0, (\timestamp -> do -- CreateThread
                        threadId <- U.parseW32
                        return $ Event timestamp (CreateThread threadId))),

                (1, (\timestamp -> do -- RunThread
                        threadId <- U.parseW32
                        return $  Event timestamp (RunThread threadId))),
                (2, (\timestamp -> do -- StopThread
                        threadId <- U.parseW32
                        blockreason <- U.parseW16
                        i <- U.parseW32
                        return $  Event timestamp (StopThread threadId
                                (if (blockreason==8)
                                    then (BlockedOnBlackHoleOwnedBy i)
                                    else (mkStopStatus blockreason))))),
                (3, (\timestamp -> do -- ThreadRunnable
                        threadId <- U.parseW32
                        return $  Event timestamp (ThreadRunnable threadId))),
                (4, (\timestamp -> do -- MigrateThread
                        threadId <- U.parseW32
                        capId    <- fromIntegral <$> U.parseW16
                        return $  Event timestamp (MigrateThread threadId capId))),
                --5-7 deprecated
                (8, (\timestamp -> do --WakeupThread
                        threadId    <- U.parseW32
                        otherCap    <- U.parseW16
                        return $  Event timestamp (WakeupThread threadId
                            (fromIntegral otherCap)))),
                (9, (\timestamp -> return $  Event timestamp StartGC)),
                (10, (\timestamp -> return $  Event timestamp EndGC)),
                (11, (\timestamp -> return $  Event timestamp RequestSeqGC)),
                (12, (\timestamp -> return $  Event timestamp RequestParGC)),
                --13/14 deprecated
                (15, (\timestamp -> do
                    threadId <-U.parseW32
                    return $  Event timestamp (CreateSparkThread threadId))),
                --16 variable sized
                (17, (\timestamp -> do
                    n_caps <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (Startup n_caps))),
                (18, (\timestamp ->do
                    blockSize <- U.parseW32
                    endTime   <- U.parseW64
                    cap       <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (EventBlock timestamp cap []))),
                (20, (\timestamp -> return $  Event timestamp GCIdle)),
                (21, (\timestamp -> return $  Event timestamp GCWork)),
                (22, (\timestamp -> return $  Event timestamp GCDone)),
                (25, (\timestamp -> do
                    capSet <- U.parseW32
                    capSetTypeId <-U.parseW16
                    let capSetType = mkCapsetType capSetTypeId
                    return $  Event timestamp (CapsetCreate capSet capSetType))),
                (26, (\timestamp -> do
                    capSet <- U.parseW32
                    return $  Event timestamp (CapsetDelete capSet))),
                (27, (\timestamp -> do
                    capSet <- U.parseW32
                    cap    <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapsetAssignCap capSet cap))),
                (28, (\timestamp -> do
                    capSet <- U.parseW32
                    cap    <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapsetRemoveCap capSet cap))),
                (32, (\timestamp -> do
                    capSet <- U.parseW32
                    pid    <- U.parseW32
                    return $  Event timestamp (OsProcessPid capSet pid))),
                (33, (\timestamp -> do
                    capSet <- U.parseW32
                    pid    <- U.parseW32
                    return $  Event timestamp (OsProcessParentPid capSet pid))),
                (34, (\timestamp -> do
                    crt <- U.parseW64
                    dud <- U.parseW64
                    ovf <- U.parseW64
                    cnv <- U.parseW64
                    gcd <- U.parseW64
                    fiz <- U.parseW64
                    rem <- U.parseW64
                    return $  Event timestamp SparkCounters{sparksCreated    = crt, sparksDud       = dud,
                                         sparksOverflowed = ovf, sparksConverted = cnv,
                                         sparksFizzled    = fiz, sparksGCd       = gcd,
                                         sparksRemaining  = rem})),
                (35, (\timestamp -> return $  Event timestamp SparkCreate)),
                (36, (\timestamp -> return $  Event timestamp SparkDud)),
                (37, (\timestamp -> return $  Event timestamp SparkOverflow)),
                (38, (\timestamp -> return $  Event timestamp SparkRun)),
                (39, (\timestamp -> do
                    vic <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (SparkSteal vic))),
                (40, (\timestamp -> return $  Event timestamp SparkFizzle)),
                (41, (\timestamp -> return $  Event timestamp SparkGC)),
                (43, (\timestamp -> do
                    capSet <- U.parseW32
                    unixEpoch <- U.parseW64
                    nanoseconds <- U.parseW32
                    return $  Event timestamp (WallClockTime capSet unixEpoch nanoseconds))),
                (45, (\timestamp -> do
                    cap <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapCreate cap))),
                (46, (\timestamp -> do
                    cap <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapDelete cap))),
                (47, (\timestamp -> do
                    cap <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapDisable cap))),
                (48, (\timestamp -> do
                    cap <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (CapEnable cap))),
                (49, (\timestamp -> do
                    cap <- U.parseW32
                    bytes <- U.parseW64
                    return $  Event timestamp (HeapAllocated cap bytes))),
                (50, (\timestamp -> do
                    cap <- U.parseW32
                    bytes <- U.parseW64
                    return $  Event timestamp (HeapSize cap bytes))),
                (51, (\timestamp -> do
                    cap <- U.parseW32
                    bytes <- U.parseW64
                    return $  Event timestamp (HeapLive cap bytes))),
                (52, (\timestamp -> do
                    cap <- U.parseW32
                    gens <- fromIntegral <$> U.parseW16
                    maxHeapSize   <- U.parseW64
                    allocAreaSize <- U.parseW64
                    mblockSize    <- U.parseW64
                    blockSize     <- U.parseW64
                    return $  Event timestamp (HeapInfoGHC cap gens maxHeapSize
                        allocAreaSize mblockSize blockSize))),
                (53, (\timestamp -> do
                    heapCapset   <- U.parseW32
                    gen          <- fromIntegral <$> U.parseW16
                    copied       <- U.parseW64
                    slop         <- U.parseW64
                    frag         <- U.parseW64
                    parNThreads  <- fromIntegral <$> U.parseW32
                    parMaxCopied <- U.parseW64
                    parTotCopied <- U.parseW64
                    return $  Event timestamp (GCStatsGHC heapCapset gen
                        copied slop frag parNThreads parMaxCopied parTotCopied))),
                (54, (\timestamp -> return $  Event timestamp GlobalSyncGC)),
                (55, (\timestamp -> do
                    taskId <- U.parseW64
                    cap    <- fromIntegral <$> U.parseW16
                    tid    <- U.parseW64
                    return $  Event timestamp (TaskCreate taskId cap (KernelThreadId tid)))),
                (56, (\timestamp -> do
                    taskId <- U.parseW64
                    cap    <- fromIntegral <$> U.parseW16
                    capNew <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (TaskMigrate taskId cap capNew))),
                (57, (\timestamp -> do
                    taskId <- U.parseW64
                    return $  Event timestamp (TaskDelete taskId))),
                (60, (\timestamp -> return $  Event timestamp EdenStartReceive)),
                (61, (\timestamp -> return $  Event timestamp EdenEndReceive)),
                (62, (\timestamp -> do
                    pid <- U.parseW32
                    return $  Event timestamp (CreateProcess pid))),
                (63, (\timestamp -> do
                    pid <- U.parseW32
                    return $  Event timestamp (KillProcess pid))),
                (64, (\timestamp -> do
                    tid <- U.parseW32
                    pid <- U.parseW32
                    return $  Event timestamp (AssignThreadToProcess tid pid))),
                (65, (\timestamp -> do
                    mid <- fromIntegral <$> U.parseW16
                    realtime <- U.parseW64
                    return $  Event timestamp (CreateMachine mid realtime))),
                (66, (\timestamp -> do
                    mid <- fromIntegral <$> U.parseW16
                    return $  Event timestamp (KillMachine mid))),
                (67, (\timestamp -> do
                    tag <- A.anyWord8
                    sP  <- U.parseW32
                    sT  <- U.parseW32
                    rM  <- U.parseW16
                    rP  <- U.parseW32
                    rIP <- U.parseW32
                    return $  Event timestamp (SendMessage { mesTag = toMsgTag tag,
                                         senderProcess = sP,
                                         senderThread = sT,
                                         receiverMachine = rM,
                                         receiverProcess = rP,
                                         receiverInport = rIP
                                       }))),
                (68, (\timestamp -> do
                    tag <- A.anyWord8
                    rP  <- U.parseW32
                    rIP <- U.parseW32
                    sM  <- U.parseW16
                    sP  <- U.parseW32
                    sT  <- U.parseW32
                    mS  <- U.parseW32
                    return $  Event timestamp (ReceiveMessage { mesTag = toMsgTag tag,
                                             receiverProcess = rP,
                                             receiverInport = rIP,
                                             senderMachine = sM,
                                             senderProcess = sP,
                                             senderThread= sT,
                                             messageSize = mS
                                           }))),
                (69, (\timestamp -> do
                    tag <- A.anyWord8
                    sP  <- U.parseW32
                    sT  <- U.parseW32
                    rP  <- U.parseW32
                    rIP <- U.parseW32
                    return $  Event timestamp (SendReceiveLocalMessage { mesTag = toMsgTag tag,
                                                     senderProcess = sP,
                                                     senderThread = sT,
                                                     receiverProcess = rP,
                                                     receiverInport = rIP
                                                   }))),
                {- from here on variable sized. -}
                (16, (\timestamp -> do
                    varDataLength <- U.parseW16
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $  Event timestamp (Message varData))),
                (19, (\timestamp -> do
                    varDataLength <- U.parseW16
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $  Event timestamp (UserMessage varData))),
                (23, (\timestamp -> do
                    varDataLength <- U.parseW16
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $  Event timestamp (Version varData))),
                (24, (\timestamp -> do
                    varDataLength <- U.parseW16-- Warning: order of fiz and gcd reversed!ord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $  Event timestamp (ProgramInvocation varData))),
                (29, (\timestamp -> do
                    varDataLength <- fromIntegral <$> U.parseW16
                    capSet <- U.parseW32
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $  Event timestamp (RtsIdentifier capSet varData))),
                (30, (\timestamp -> do
                    varDataLength <- fromIntegral <$> U.parseW16
                    capSet <- U.parseW32
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $  Event timestamp (ProgramArgs capSet (splitNull varData)))),
                (31, (\timestamp -> do
                    varDataLength <- fromIntegral <$> U.parseW16
                    capSet <- U.parseW32
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $  Event timestamp (ProgramEnv capSet (splitNull varData)))),
                (42, (\timestamp -> do
                    varDataLength <- fromIntegral <$> U.parseW16
                    string <- C.unpack <$> A.take (varDataLength - 4)
                    stringId <- U.parseW32
                    return $  Event timestamp (InternString string stringId))),
                (44, (\timestamp -> do
                    varDataLength <- fromIntegral <$> U.parseW16-- Warning: order of fiz and gcd reversed!ord16be
                    threadId <- U.parseW32
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $  Event timestamp (ThreadLabel threadId varData))),
                (58, (\timestamp -> do
                    varDataLength <- U.parseW16-- Warning: order of fiz and gcd reversed!ord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $  Event timestamp (UserMarker varData)))
                         ]

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

