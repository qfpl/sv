{-|
Module      : Data.Sv.Print
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Printing is the process of turning an 'Data.Sv.Syntax.Sv.Sv' into a textual
representation, such as a 'Data.ByteString.ByteString'.

If you want to turn your data type into a textual representation, you should
look instead at "Data.Sv.Encode"
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
, module Data.Sv.Print.Options
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Print.Options
import Data.Sv.Print.Internal
import Data.Sv.Syntax.Sv (Sv (Sv))

-- | Converts an 'Sv' to a ByteString 'Builder'. Useful if you want to concatenate other
-- text before or after.
svToBuilder :: PrintOptions s -> Sv s -> Builder
svToBuilder opts (Sv sep h rs e) =
  foldMap (printHeader opts sep) h <> printRecords opts sep rs <> foldMap printNewline e

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
writeSvToHandle :: Handle -> Sv ByteString -> IO ()
writeSvToHandle = writeSvToHandle' defaultPrintOptions

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
writeSvToFile :: FilePath -> Sv ByteString -> IO ()
writeSvToFile = writeSvToFile' defaultPrintOptions

-- | Writes an sv to a file handle. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to the handle.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToHandle' :: PrintOptions s -> Handle -> Sv s -> IO ()
writeSvToHandle' opts h sv = hPutBuilder h (svToBuilder opts sv)

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
--
-- This version is polymorphic, but as a penalty you have to tell me how to
-- get a Bytestring 'Builder'.
writeSvToFile' :: PrintOptions s -> FilePath -> Sv s -> IO ()
writeSvToFile' opts fp sv = do
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  writeSvToHandle' opts h sv
  hClose h

-- | Print an Sv to a 'Data.ByteString.ByteString' value.
printSv :: Sv ByteString -> ByteString
printSv = printSv' defaultPrintOptions

-- | Print an Sv to a lazy 'Data.ByteString.Lazy.ByteString' value.
printSvLazy :: Sv ByteString -> LBS.ByteString
printSvLazy = printSvLazy' defaultPrintOptions

-- | Converts the given 'Sv' into a strict 'Data.ByteString.ByteString'
printSv' :: PrintOptions s -> Sv s -> ByteString
printSv' opts = LBS.toStrict . printSvLazy' opts

-- | Converts the given 'Sv' into a lazy 'Data.ByteString.Lazy.ByteString'
printSvLazy' :: PrintOptions s -> Sv s -> LBS.ByteString
printSvLazy' opts = toLazyByteString . svToBuilder opts

-- | Print an Sv containing 'Text' to a 'Data.ByteString.ByteString'
printSvText :: Sv Text -> ByteString
printSvText = printSv' textPrintOptions

-- | Print an Sv containing 'Text' to a 'Data.ByteString.Lazy.ByteString'
printSvTextLazy :: Sv Text -> LBS.ByteString
printSvTextLazy = printSvLazy' textPrintOptions
