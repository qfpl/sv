{-|
Module      : Data.Sv.Print.Internal
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module is considered an implementation detail.
As the "Internal" module name suggests, this module is exempt from
the PVP, so depend on it at your own risk!
These functions exist to be called by "Data.Sv.Print".
-}

module Data.Sv.Print.Internal (
  printNewline
  , printField
  , printSpaced
  , printRecord
  , printRecords
  , printHeader
) where

import Control.Lens (review, view)
import Data.Bifoldable (bifoldMap)
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (intercalate1)

import Data.Sv.Print.Options
import Data.Sv.Syntax.Field (Field (Quoted, Unquoted), SpacedField)
import Data.Sv.Syntax.Record (Record (Record), Records (Records, EmptyRecords))
import Data.Sv.Syntax.Sv (Header (Header), Separator)
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

-- | Convert a 'Newline' to a ByteString 'Builder'
printNewline :: Newline -> Builder
printNewline = Builder.lazyByteString . newlineToString

-- | Convert a 'Field' to a ByteString 'Builder'
printField :: PrintOptions s -> Field s -> Builder
printField opts f =
  case f of
    Unquoted s ->
      view build opts s
    Quoted q s ->
      let qc = quoteToString q
          contents = view build opts $ view escape opts (review quoteChar q) s
      in  qc <> contents <> qc

-- | Convert a 'SpacedField' to a ByteString 'Builder'
printSpaced :: PrintOptions s -> SpacedField s -> Builder
printSpaced opts (Spaced b t a) =
  let spc = foldMap (Builder.charUtf8 . spaceToChar)
  in  spc b <> printField opts a <> spc t

-- | Convert a 'Record' to a ByteString 'Builder'
printRecord :: PrintOptions s -> Separator -> Record s -> Builder
printRecord opts sep (Record fs) =
  intercalate1 (Builder.charUtf8 sep) (fmap (printSpaced opts) fs)

-- | Convert 'Records' to a ByteString 'Builder'.
printRecords :: PrintOptions s -> Separator -> Records s -> Builder
printRecords opts sep rs = case rs of
  EmptyRecords -> mempty
  Records a as ->
    printRecord opts sep a <> foldMap (bifoldMap printNewline (printRecord opts sep)) as

-- | Convert 'Header' to a ByteString 'Builder'.
printHeader :: PrintOptions s -> Separator -> Header s -> Builder
printHeader opts sep (Header r n) = printRecord opts sep r <> printNewline n
