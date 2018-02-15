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
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (intercalate1)

import Data.Sv.Syntax.Field (Field (Quoted, Unquoted), SpacedField)
import Data.Sv.Syntax.Record (Record (Record), Records (Records, EmptyRecords))
import Data.Sv.Syntax.Sv (Header (Header), Separator)
import Text.Babel (Textual (toByteStringBuilder), singleton)
import Text.Escape (getRawEscaped, Escapable (escape_))
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

-- | Convert a 'Newline' to a ByteString 'Builder'
printNewline :: Newline -> Builder
printNewline n = toByteStringBuilder (newlineText n :: LB.ByteString)

-- | Convert a 'Field' to a ByteString 'Builder'
printField :: Escapable s => Field s -> Builder
printField f =
  case f of
    Unquoted s ->
      toByteStringBuilder s
    Quoted q s ->
      let qc = quoteToString q
          contents = toByteStringBuilder $ getRawEscaped $ escape_ (review quoteChar q) s
      in  qc <> contents <> qc

-- | Convert a 'SpacedField' to a ByteString 'Builder'
printSpaced :: Escapable s => SpacedField s -> Builder
printSpaced (Spaced b t a) =
  let spc = foldMap (singleton . spaceToChar)
  in  spc b <> printField a <> spc t

-- | Convert a 'Record' to a ByteString 'Builder'
printRecord :: Escapable s => Separator -> Record s -> Builder
printRecord sep (Record fs) =
  intercalate1 (singleton sep) (fmap printSpaced fs)

-- | Convert 'Records' to a ByteString 'Builder'.
printRecords :: Escapable s => Separator -> Records s -> Builder
printRecords sep rs = case rs of
  EmptyRecords -> mempty
  Records a as ->
    printRecord sep a <> foldMap (bifoldMap printNewline (printRecord sep)) as

-- | Convert 'Header' to a ByteString 'Builder'.
printHeader :: Escapable s => Separator -> Header s -> Builder
printHeader sep (Header r n) = printRecord sep r <> printNewline n
