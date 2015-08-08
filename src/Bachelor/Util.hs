
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

{-
 - Data.Attoparsec does not provide any code to convert multibyte
 - ByteStrings into unsigned binary types. The code provided by
 - attoparsec-binary and binary is rather computation heavy.
 - This module uses the C FFI and a small C library to interpret the
 - ByteString memory as an unsigned integer, and then restores the correct
 - byte order (if needed, which in *nix) should just be a copy.
 - -}

module Bachelor.Util where

import Foreign.C
import System.IO.Unsafe
import Data.Word
import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

foreign import ccall unsafe "fastconvert.h w16be" w16be :: CString -> IO CUShort
foreign import ccall unsafe "fastconvert.h w32be" w32be :: CString -> IO CUInt
foreign import ccall unsafe "fastconvert.h w64be" w64be :: CString -> IO CULong

w16 :: B.ByteString -> Word16
w16 bs = case (unsafePerformIO $ C.useAsCString bs w16be) of
        (CUShort x) -> x

w32 :: B.ByteString -> Word32
w32 bs = case (unsafePerformIO $ C.useAsCString bs w32be) of
        (CUInt x) -> x

w64 :: B.ByteString -> Word64
w64 bs = case (unsafePerformIO $ C.useAsCString bs w64be) of
        (CULong x) -> x

parseW16 :: A.Parser Word16
parseW16 = w16 <$> A.take 2
{-# INLINE parseW16 #-}

parseW32 :: A.Parser Word32
parseW32 = w32 <$> A.take 4
{-# INLINE parseW32 #-}

parseW64 :: A.Parser Word64
parseW64 = w64 <$> A.take 8
{-# INLINE parseW64 #-}
