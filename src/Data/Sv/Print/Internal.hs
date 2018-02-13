module Data.Sv.Print.Internal where

import Control.Lens (view, review)
import Data.Bifoldable (bifoldMap)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder as Builder
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (intercalate1)
import Data.Separated (Pesarated1)

import Data.Sv.Syntax.Field (Field (Quoted, Unquoted))
import Data.Sv.Syntax.Record (Record (Record), Records, theRecords)
import Data.Sv.Syntax.Sv (Header (Header), Separator)
import Text.Babel (Textual (toByteStringBuilder), singleton)
import Text.Escape (getRawEscaped, Escapable (escape_))
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

printNewline :: Newline -> Builder
printNewline n = toByteStringBuilder (newlineText n :: LB.ByteString)

printField :: Escapable s => Field s -> Builder
printField f =
  case f of
    Unquoted s ->
      toByteStringBuilder s
    Quoted q s ->
      let qc = quoteToString q
          contents = toByteStringBuilder $ getRawEscaped $ escape_ (review quoteChar q) s
      in  qc <> contents <> qc

printSpaced :: Escapable s => Spaced (Field s) -> Builder
printSpaced (Spaced b t a) =
  let spc = foldMap (singleton . spaceToChar)
  in  spc b <> printField a <> spc t

printRecord :: Escapable s => Separator -> Record s -> Builder
printRecord sep (Record fs) =
  intercalate1 (singleton sep) (fmap printSpaced fs)

printPesarated1 :: Escapable s => Separator -> Pesarated1 Newline (Record s) -> Builder
printPesarated1 sep = bifoldMap printNewline (printRecord sep)

printRecords :: Escapable s => Separator -> Records s -> Builder
printRecords sep = foldMap (printPesarated1 sep) . view theRecords

printHeader :: Escapable s => Separator -> Header s -> Builder
printHeader sep (Header r n) = printRecord sep r <> printNewline n
