This is -*- Literate Haskell -*-

Our own wrappers around zip file handling in Haskell.

Written by Oleg Lobachev, 2010.

\begin{code}
module Bachelor.TinyZipper where

import Prelude hiding (readFile)
import Codec.Archive.Zip
import Data.ByteString.Lazy (ByteString, readFile)
import System.IO.Unsafe (unsafePerformIO) -- yeah!
import Data.Maybe
\end{code}

We need to read files from a zip archive, if possible: without unpacking it.

Ideal is something like String (with zip file name) -> [FileContents].

\begin{code}
type FileContents = ByteString
readZip :: FilePath -> IO (Either String [FileContents])
readZip filePath = do
    zippedFile <- readFile filePath
    let archive  = toArchive zippedFile
        fileList = filesInArchive archive
        entries  = catMaybes $ zipWith (findEntryByPath) fileList (repeat archive)
        files    = map (fromEntry) entries
    return $ seq files $ Right files
\end{code}

This file was taken directly out of EdenTV
