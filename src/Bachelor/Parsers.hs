module Bachelor.Parsers where
import GHC.RTS.Events
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as Bin
import qualified Data.HashMap.Strict as M
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Applicative

-- copy/pasted from GHC.RTS.EventTypes, because these are not exported.
type EventTypeNum     = Word16
type EventTypeDescLen = Word32
type EventTypeDesc    = String
type EventTypeSize    = Word16

type Capset = Word32

type SizeTable = M.HashMap EventTypeNum (Maybe EventTypeSize)

eventLogParser :: A.Parser EventLog
eventLogParser = do
    eventLogHeader <- headerParser
    let sizeTable = mkSizeTable eventLogHeader
    return undefined

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
    id_    <- Bin.anyWord16be
    eventTypeSize   <- Bin.anyWord16be
    sizeName <- Bin.anyWord32be
    name   <- A.take (fromIntegral sizeName)
    sizeExtraInfo <- fromIntegral <$> Bin.anyWord32be
    _ <- A.take sizeExtraInfo
    let v = eventTypeSize == 0xFFFF
    return $ EventType id_ (C.unpack name) (if v then Nothing else Just eventTypeSize)

testHeader = do
    bs <- B.readFile "/home/basti/bachelor/traces/mergesort_large/mergesort#80.eventlog"
    let res = A.parse headerParser bs
    case res of
        (A.Done _ r) -> mapM_ print $ eventTypes r
        _             -> error "parsing failed"

parseEventStrem :: SizeTable -> A.Parser [Event]
parseEventStrem st = do
    _ <- A.string $ C.pack "datb"
    eventList <- A.manyTill (parseEvent st) (A.try (Bin.word16be 0xFFFF)) --parse Events until the
                                                      --end Marker is reached.
    return []

-- parses relevant Events, skips otherwise
-- This parser is not compatible with eventlogs created wtype Capset   = Word32
parseEvent :: SizeTable -> A.Parser Event
parseEvent st = do
    type_     <- Bin.anyWord16be -- the type (spec) of the event
    timestamp <- Bin.anyWord64be -- the timestamp
    let maybeSize = st M.! type_ --lookup the size of the event in the table
    case maybeSize of
        (Just s) ->
            case type_ of
                0 -> do -- CreateThread
                        threadId <- Bin.anyWord32be
                        return $ Event timestamp (CreateThread threadId)
                1 -> do -- RunThread
                        threadId <- Bin.anyWord32be
                        return $ Event timestamp (RunThread threadId)
                2 -> do -- StopThread
                        threadId <- Bin.anyWord32be
                        blockreason <- Bin.anyWord16be
                        _ <- Bin.anyWord32be
                            -- used in older ghcs by BlockedOnBlackHoleOwnedBy
                            -- ignored here.
                        return $ Event timestamp (StopThread threadId
                                (mkStopStatus blockreason))
                3 -> do -- ThreadRunnable
                        threadId <- Bin.anyWord32be
                        return $ Event timestamp (ThreadRunnable threadId)
                4 -> do -- MigrateThread
                        threadId <- Bin.anyWord32be
                        capId    <- fromIntegral <$> Bin.anyWord16be
                        return $ Event timestamp (MigrateThread threadId capId)
                --5-7 deprecated
                8 -> do --WakeupThread
                        threadId    <- Bin.anyWord32be
                        otherCap    <- Bin.anyWord16be
                        return $ Event timestamp (MigrateThread threadId
                            (fromIntegral otherCap))
                9 -> return $ Event timestamp StartGC
                10-> return $ Event timestamp EndGC
                11-> return $ Event timestamp RequestSeqGC
                12-> return $ Event timestamp RequestParGC
                --13/14 deprecated
                15-> do
                    threadId <-Bin.anyWord32be
                    return $ Event timestamp (CreateSparkThread threadId)
                --16 variable sized
                17-> do
                    cap <- fromIntegral <$> Bin.anyWord16be
                    return $ Event timestamp (CapCreate cap)
                18-> do --TODO: skip this if we dont care for this capability!
                    return undefined
                20-> return $ Event timestamp GCIdle
                21-> return $ Event timestamp GCWork
                22-> return $ Event timestamp GCDone
                25-> do
                    capSet <- Bin.anyWord32be
                    capSetTypeId <-Bin.anyWord16be
                    let capSetType = (mkCapsetType capSetTypeId)
                    return $ Event timestamp (CapsetCreate capSet capSetType)
                26-> do
                    capSet <- Bin.anyWord32be
                    return $ Event timestamp (CapsetDelete capSet)
                27-> do
                    capSet <- Bin.anyWord32be
                    cap    <- fromIntegral <$> Bin.anyWord16be
                    return $ Event timestamp (CapsetAssignCap capSet cap)
                28 -> do
                    capSet <- Bin.anyWord32be
                    cap    <- fromIntegral <$> Bin.anyWord16be
                    return $ Event timestamp (CapsetRemoveCap capSet cap)
                32 -> do
                    capSet <- Bin.anyWord32be
                    pid    <- Bin.anyWord32be
                    return $ Event timestamp (OsProcessPid capSet pid)
                33 -> do
                    capSet <- Bin.anyWord32be
                    pid    <- Bin.anyWord32be
                    return $ Event timestamp (OsProcessParentPid capSet pid)
                34 -> do
                    crt <- Bin.anyWord64be
                    dud <- Bin.anyWord64be
                    ovf <- Bin.anyWord64be
                    cnv <- Bin.anyWord64be
                    gcd <- Bin.anyWord64be
                    fiz <- Bin.anyWord64be
                    rem <- Bin.anyWord64be
                    return $ Event timestamp SparkCounters{sparksCreated    = crt, sparksDud       = dud,
                                         sparksOverflowed = ovf, sparksConverted = cnv,
                                         sparksFizzled    = fiz, sparksGCd       = gcd,
                                         sparksRemaining  = rem}
                35-> return $ Event timestamp SparkCreate
                36-> return $ Event timestamp SparkDud
                37-> return $ Event timestamp SparkOverflow
                38-> return $ Event timestamp SparkRun

                _ -> do --for all event types not yet implemented.
                        _ <- A.take (fromIntegral s)
                        return $ Event timestamp (UnknownEvent type_)
        Nothing -> do
            case type_ of
                16 -> do
                    varDataLength <- Bin.anyWord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $ Event timestamp (Message varData)
                19 -> do
                    varDataLength <- Bin.anyWord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $ Event timestamp (UserMessage varData)
                23 -> do
                    varDataLength <- Bin.anyWord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $ Event timestamp (Version varData)
                24 -> do
                    varDataLength <- Bin.anyWord16be-- Warning: order of fiz and gcd reversed!ord16be
                    varData <- C.unpack <$> A.take (fromIntegral varDataLength)
                    return $ Event timestamp (Version varData)
                29 -> do
                    varDataLength <- fromIntegral <$> Bin.anyWord16be
                    capSet <- Bin.anyWord32be
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $ Event timestamp (RtsIdentifier capSet varData)
                30 -> do
                    varDataLength <- fromIntegral <$> Bin.anyWord16be
                    capSet <- Bin.anyWord32be
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $ Event timestamp (ProgramArgs capSet (splitNull varData))
                31 -> do
                    varDataLength <- fromIntegral <$> Bin.anyWord16be
                    capSet <- Bin.anyWord32be
                    varData <- C.unpack <$> A.take (varDataLength - 4)
                    return $ Event timestamp (ProgramEnv capSet (splitNull varData))
                _ -> do
                    eventSize  <- Bin.anyWord16be
                    _  <- A.take $ fromIntegral eventSize
                    return $ Event timestamp (UnknownEvent type_)

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
