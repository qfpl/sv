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
, writeSvToFile
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import System.IO (BufferMode (BlockBuffering), hClose, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Print.Internal
import Data.Sv.Syntax.Sv (Sv (Sv))
import Text.Escape (Escapable)

-- | Converts an 'Sv' to a ByteString 'Builder'. Useful if you want to concatenate other
-- text before or after.
svToBuilder :: Escapable s => Sv s -> Builder
svToBuilder (Sv sep h rs e) =
  foldMap (printHeader sep) h <> printRecords sep rs <> foldMap printNewline e

-- | Writes an sv to a file. This goes directly from a 'Builder', so it is
-- more efficient than calling 'printSv' or 'printSvLazy' and writing the
-- result to a file.
writeSvToFile :: Escapable s => FilePath -> Sv s -> IO ()
writeSvToFile fp sv = do
  let b = svToBuilder sv
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  hPutBuilder h b
  hClose h

-- | Converts the given 'Sv' into a strict 'Data.ByteString.ByteString'
printSv :: Escapable s => Sv s -> ByteString
printSv = LBS.toStrict . printSvLazy

-- | Converts the given 'Sv' into a lazy 'Data.ByteString.Lazy.ByteString'
printSvLazy :: Escapable s => Sv s -> LBS.ByteString
printSvLazy = toLazyByteString . svToBuilder
