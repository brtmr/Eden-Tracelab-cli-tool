{-# LANGUAGE CPP #-}
module Bachelor.SeqParse.PreParse where

{-
 - Pre-Parses an eventlog file and returns information about the
 - event blocks within.
 - -}

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import Control.Monad (when)
import Control.Monad.Except
import Data.Binary.Get
import Data.ByteString as B (unpack, ByteString)
import Data.ByteString.Lazy as LB (readFile, ByteString, hGet)
import Data.Char (chr)
import Data.Int
import Data.Word
import Debug.Trace
import GHC.RTS.Events
import System.IO
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M

-- | Contains skip actions for skipping event blocks belonging to
-- | other caps.
type SkipMap = M.HashMap ByteOffset Int

-- | data type for block information
data BlockInfo = BlockInfo {
    capNo :: Int, -- ^ the Capability this event belongs to
    position :: ByteOffset, -- ^ the position this block occured
    starttime :: Word64, -- ^ starttime of the block in ns
    endtime :: Word64,    -- ^ endtime of the block in ns
    bSize :: Word32 -- ^ size of the eventblock in bytes
    } deriving Show

-- | Computes a HashMap with positions as keys and length of the
-- | skip as values
mkSkipMap :: Int -> [BlockInfo] -> SkipMap
mkSkipMap n blockinfo = F.foldl' insertOtherBlock myBlockMap otherBlocks
    where
        myBlockMap    = F.foldl' insertMyBlock M.empty myBlocks
        myBlocks      = filter (\x -> capNo x==n) blockinfo
        otherBlocks   = filter (\x -> capNo x/=n) blockinfo
        insertMyBlock m bi    = M.insert (position bi) (24) m
                                       -- this is a block belonging to the cap
                                       -- we are interested in. just skip
                                       -- the block event
        insertOtherBlock m bi = M.insert (position bi)
                                       -- this is a block belonging to another
                                       -- cap. skip it entirely.
            (fromIntegral $ bSize bi) m

-- | the main function for finding blocks.
findBlocks :: LB.ByteString -> IO [BlockInfo]
findBlocks bs = do
    let header_res = runGetOrFail (skipHeader >> (skip 4)) bs --'datb'
    case header_res of
        Left  (_,_,err) -> return $ error err
        Right (bs' ,offset, _) -> do
            return $ findBlocks' bs' offset

-- | helper function to parse the eventblocks after consuming the header.
findBlocks' :: LB.ByteString -> ByteOffset -> [BlockInfo]
findBlocks' bs offset =
    if over
        then []
        else case (runGetOrFail getEventBlocksNoHeader bs) of
            (Left  (_,_,err)) -> error err
            (Right (bs', offset', b)) ->
                (b {position = offset}) : (findBlocks' bs' (offset'+offset))
    where over = runGet getEndMarker bs

-- | to check for the end of the event stream
getEndMarker :: Get Bool
getEndMarker = do
    endmaker <- lookAhead getWord16be
    return (endmaker == 0xffff)

-- | Get action that parses a single eventblock
getEventBlocksNoHeader :: Get BlockInfo
getEventBlocksNoHeader = do
    type_      <- getWord16be
    if (type_ /= EVENT_BLOCK_MARKER)
        then return $ error "not an event block!"
        else do
            starttime  <- getWord64be
            size       <- getWord32be
            endtime    <- getWord64be
            capNo      <- getWord16be
            skip $ fromIntegral (size-24)
            return $ BlockInfo
                (fromIntegral capNo)
                0 -- to be set after running
                starttime
                endtime
                size

-- | Get Action that returns nothing but consumes the header of an
-- | eventLog file.
skipHeader :: Get ()
skipHeader = do
    skip 8 -- 'hdrb' 'hetb'
    marker <- skipEventTypeDeclarations
    if (marker == EVENT_HEADER_END) then do
        return()
    else
        return $ error "EVENT_HEADER_END marker not found"

skipEventTypeDeclarations :: Get Word32
skipEventTypeDeclarations = do
    marker <- getWord32be
    if (marker == EVENT_ET_BEGIN) then do
            skip 2 --id
            skip 2 --size
            description_size <- getWord32be
            skip $ fromIntegral description_size
            extra_info_size <- getWord32be
            skip $ fromIntegral extra_info_size
            skip 4 -- "ete\NUL"
            skipEventTypeDeclarations
        else if (marker == EVENT_HET_END) then do
                marker' <- getWord32be
                return marker'
            else do
                return $ error "EVENT_HET_END marker not found"
