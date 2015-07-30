module Bachelor.Parsers where
import GHC.RTS.Events (Timestamp,ThreadId,EventInfo(..),EventType(..), Event(..), Header(..), Data, EventLog(..), Header)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as Bin
import qualified Data.HashMap.Strict as M
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe

-- copy/pasted from GHC.RTS.EventTypes, because these are not exported.
type EventTypeNum     = Word16
type EventTypeDescLen = Word32
type EventTypeDesc    = String
type EventTypeSize    = Word16

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
    sizeExtraInfo <- Bin.anyWord32be
    _ <- A.take (fromIntegral sizeExtraInfo) -- the file format defines a field
                                             -- for extra information that is
                                             -- never used.
    _ <- A.string $ C.pack "ete\NUL" --end event type
    let v = fromIntegral eventTypeSize == 0xFFFF
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
    A.manyTill (parseEvent st) (A.try (Bin.word16be 0xFFFF)) --parse Events until the
                                                      --end Marker is reached.
    return []

-- parses relevant Events, skips otherwise
parseEvent :: SizeTable -> A.Parser (Maybe Event)
parseEvent st = do
    type_     <- Bin.anyWord16be -- the type (spec) of the event
    timestamp <- Bin.anyWord64be -- the timestamp
    let maybeSize = st M.! type_ --lookup the size of the event in the table
    case maybeSize of
        (Just s) -> do
            case type_ of
                0 -> do -- CreateThread
                        threadId <- Bin.anyWord32be
                        return (Just (Event timestamp (CreateThread threadId)))
                1 -> do -- RunThread
                        threadId <- Bin.anyWord32be
                        return (Just (Event timestamp (RunThread threadId)))
                2 -> do -- StopThread
                        return Nothing
                _ -> do --for all event types not yet implemented.
                        _ <- A.take (fromIntegral s)
                        return Nothing
        Nothing ->
            case type_ of
                 _ -> do
                    eventSize  <- Bin.anyWord16be
                    eventData  <- A.take $ fromIntegral eventSize
                    return undefined
