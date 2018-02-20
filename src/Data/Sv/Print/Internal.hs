{-|
Module      : Data.Sv.Print.Internal
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module is considered an implementation detail and is likely to change at
any time. Depend on it at your own peril.
These functions exist to be called by 'Data.Sv.Print'
-}

module Data.Sv.Print.Internal where

import Control.Lens (review)
import Data.Bifoldable (bifoldMap)
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (intercalate1)

import Data.Sv.Syntax.Field (Field (Quoted, Unquoted), SpacedField)
import Data.Sv.Syntax.Record (Record (Record), Records (Records, EmptyRecords))
import Data.Sv.Syntax.Sv (Header (Header), Separator)
import Text.Escape (getRawEscaped, Escapable (escape_))
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

-- | Convert a 'Newline' to a ByteString 'Builder'
printNewline :: Newline -> Builder
printNewline = Builder.lazyByteString . newlineText

-- | Convert a 'Field' to a ByteString 'Builder'
printField :: Escapable s => (s -> Builder) -> Field s -> Builder
printField build f =
  case f of
    Unquoted s ->
      build s
    Quoted q s ->
      let qc = quoteToString q
          contents = build $ getRawEscaped $ escape_ (review quoteChar q) s
      in  qc <> contents <> qc

-- | Convert a 'SpacedField' to a ByteString 'Builder'
printSpaced :: Escapable s => (s -> Builder) -> SpacedField s -> Builder
printSpaced build (Spaced b t a) =
  let spc = foldMap (Builder.charUtf8 . spaceToChar)
  in  spc b <> printField build a <> spc t

-- | Convert a 'Record' to a ByteString 'Builder'
printRecord :: Escapable s => Separator -> (s -> Builder) -> Record s -> Builder
printRecord sep build (Record fs) =
  intercalate1 (Builder.charUtf8 sep) (fmap (printSpaced build) fs)

-- | Convert 'Records' to a ByteString 'Builder'.
printRecords :: Escapable s => Separator -> (s -> Builder) -> Records s -> Builder
printRecords sep build rs = case rs of
  EmptyRecords -> mempty
  Records a as ->
    printRecord sep build a <> foldMap (bifoldMap printNewline (printRecord sep build)) as

-- | Convert 'Header' to a ByteString 'Builder'.
printHeader :: Escapable s => Separator -> (s -> Builder) -> Header s -> Builder
printHeader sep build (Header r n) = printRecord sep build r <> printNewline n
