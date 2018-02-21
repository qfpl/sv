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
import Text.Escape (Escaper, escapeUtf8, escapeText)

-- | Converts an 'Sv' to a ByteString 'Builder'. Useful if you want to concatenate other
-- text before or after.
svToBuilder :: Escaper s -> (s -> Builder) -> Sv s -> Builder
svToBuilder escape build (Sv sep h rs e) =
  foldMap (printHeader escape sep build) h <> printRecords escape sep build rs <> foldMap printNewline e

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
writeSvToHandle :: Handle -> Sv ByteString -> IO ()
writeSvToHandle = writeSvToHandle' escapeUtf8 byteString

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
writeSvToFile :: FilePath -> Sv ByteString -> IO ()
writeSvToFile = writeSvToFile' escapeUtf8 byteString

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToHandle' :: Escaper s -> (s -> Builder) -> Handle -> Sv s -> IO ()
writeSvToHandle' escape build h sv = hPutBuilder h (svToBuilder escape build sv)

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToFile' :: Escaper s -> (s -> Builder) -> FilePath -> Sv s -> IO ()
writeSvToFile' escape build fp sv = do
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  writeSvToHandle' escape build h sv
  hClose h

-- | Print an Sv to a 'Data.ByteString.ByteString' value.
printSv :: Sv ByteString -> ByteString
printSv = printSv' escapeUtf8 byteString

-- | Print an Sv to a lazy 'Data.ByteString.Lazy.ByteString' value.
printSvLazy :: Sv ByteString -> LBS.ByteString
printSvLazy = printSvLazy' escapeUtf8 byteString

-- | Converts the given 'Sv' into a strict 'Data.ByteString.ByteString'
printSv' :: Escaper s -> (s -> Builder) -> Sv s -> ByteString
printSv' escape build = LBS.toStrict . printSvLazy' escape build

-- | Converts the given 'Sv' into a lazy 'Data.ByteString.Lazy.ByteString'
printSvLazy' :: Escaper s -> (s -> Builder) -> Sv s -> LBS.ByteString
printSvLazy' escape build = toLazyByteString . svToBuilder escape build

-- | Print an Sv containing 'Text' to a 'Data.ByteString.ByteString'
printSvText :: Sv Text -> ByteString
printSvText = printSv' escapeText (byteString . encodeUtf8)

-- | Print an Sv containing 'Text' to a 'Data.ByteString.Lazy.ByteString'
printSvTextLazy :: Sv Text -> LBS.ByteString
printSvTextLazy = printSvLazy' escapeText (byteString . encodeUtf8)
