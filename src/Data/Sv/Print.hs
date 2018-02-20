{-|
Module      : Data.Sv.Print
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Print (
  printSv
, printSvLazy
, printSv'
, printSvLazy'
, printSvText
, printSvTextLazy
, writeSvToFile
, writeSvToHandle
, writeSvToFile'
, writeSvToHandle'
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Print.Internal
import Data.Sv.Syntax.Sv (Sv (Sv))
import Text.Escape (Escapable)

-- | Converts an 'Sv' to a ByteString 'Builder'. Useful if you want to concatenate other
-- text before or after.
svToBuilder :: Escapable s => (s -> Builder) -> Sv s -> Builder
svToBuilder build (Sv sep h rs e) =
  foldMap (printHeader sep build) h <> printRecords sep build rs <> foldMap printNewline e

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
writeSvToHandle :: Handle -> Sv ByteString -> IO ()
writeSvToHandle = writeSvToHandle' byteString

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
writeSvToFile :: FilePath -> Sv ByteString -> IO ()
writeSvToFile = writeSvToFile' byteString

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToHandle' :: Escapable s => (s -> Builder) -> Handle -> Sv s -> IO ()
writeSvToHandle' build h sv = hPutBuilder h (svToBuilder build sv)

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToFile' :: Escapable s => (s -> Builder) -> FilePath -> Sv s -> IO ()
writeSvToFile' build fp sv = do
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  writeSvToHandle' build h sv
  hClose h

-- | Print an Sv to a 'Data.ByteString.ByteString' value.
printSv :: Sv ByteString -> ByteString
printSv = printSv' byteString

-- | Print an Sv to a lazy 'Data.ByteString.Lazy.ByteString' value.
printSvLazy :: Sv ByteString -> LBS.ByteString
printSvLazy = printSvLazy' byteString

-- | Converts the given 'Sv' into a strict 'Data.ByteString.ByteString'
printSv' :: Escapable s => (s -> Builder) -> Sv s -> ByteString
printSv' build = LBS.toStrict . printSvLazy' build

-- | Converts the given 'Sv' into a lazy 'Data.ByteString.Lazy.ByteString'
printSvLazy' :: Escapable s => (s -> Builder) -> Sv s -> LBS.ByteString
printSvLazy' build = toLazyByteString . svToBuilder build

-- | Print an Sv containing 'Text' to a 'Data.ByteString.ByteString'
printSvText :: Sv Text -> ByteString
printSvText = printSv' (byteString . encodeUtf8)

-- | Print an Sv containing 'Text' to a 'Data.ByteString.Lazy.ByteString'
printSvTextLazy :: Sv Text -> LBS.ByteString
printSvTextLazy = printSvLazy' (byteString . encodeUtf8)