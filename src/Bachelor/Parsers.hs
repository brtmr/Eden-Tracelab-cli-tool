module Bachelor.Parsers where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as Bin
import qualified Data.HashMap.Strict as M
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

type EventTypeId = Word16
data EventType = EventType {
	id_ :: EventTypeId,
	size :: Word16,
	name :: String,
	extraInfo :: String,
	variable :: Bool
}

type HeaderInfo = M.HashMap EventTypeId EventType

headerParser :: A.Parser HeaderInfo
headerParser = do
	_ <- A.string $ C.pack "hdrb" -- begin header
	_ <- A.string $ C.pack "hetb" -- begin event type list
	typeList <- A.many1' eventTypeParser
	_ <- A.string $ C.pack "hete" -- end header
	_ <- A.string $ C.pack "hdre" -- end event type list
	return $ foldr (\e m -> M.insert (id_ e) e m) M.empty typeList

eventTypeParser :: A.Parser EventType
eventTypeParser = do
	id_    <- Bin.anyWord16be
	size   <- Bin.anyWord16be
	sizeName <- Bin.anyWord32be
	name   <- A.take (fromIntegral sizeName)
	sizeExtraInfo <- Bin.anyWord32be
	extraInfo <- A.take (fromIntegral sizeExtraInfo)
	return $ EventType
		id_
		size
		(C.unpack name)
		(C.unpack extraInfo)
		False
