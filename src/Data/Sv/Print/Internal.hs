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
import Text.Escape (Escaper)
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

-- | Convert a 'Newline' to a ByteString 'Builder'
printNewline :: Newline -> Builder
printNewline = Builder.lazyByteString . newlineText

-- | Convert a 'Field' to a ByteString 'Builder'
printField :: Escaper s -> (s -> Builder) -> Field s -> Builder
printField escape build f =
  case f of
    Unquoted s ->
      build s
    Quoted q s ->
      let qc = quoteToString q
          contents = build $ escape (review quoteChar q) s
      in  qc <> contents <> qc

-- | Convert a 'SpacedField' to a ByteString 'Builder'
printSpaced :: Escaper s -> (s -> Builder) -> SpacedField s -> Builder
printSpaced escape build (Spaced b t a) =
  let spc = foldMap (Builder.charUtf8 . spaceToChar)
  in  spc b <> printField escape build a <> spc t

-- | Convert a 'Record' to a ByteString 'Builder'
printRecord :: Escaper s -> Separator -> (s -> Builder) -> Record s -> Builder
printRecord escape sep build (Record fs) =
  intercalate1 (Builder.charUtf8 sep) (fmap (printSpaced escape build) fs)

-- | Convert 'Records' to a ByteString 'Builder'.
printRecords :: Escaper s -> Separator -> (s -> Builder) -> Records s -> Builder
printRecords escape sep build rs = case rs of
  EmptyRecords -> mempty
  Records a as ->
    printRecord escape sep build a <> foldMap (bifoldMap printNewline (printRecord escape sep build)) as

-- | Convert 'Header' to a ByteString 'Builder'.
printHeader :: Escaper s -> Separator -> (s -> Builder) -> Header s -> Builder
printHeader escape sep build (Header r n) = printRecord escape sep build r <> printNewline n
