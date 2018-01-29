module Data.Sv.Print (
  printSv
, printField
, printSpaced
, writeSvToFile
, displaySv
, displaySvLazy
) where

import Control.Lens (view)
import Data.Bifoldable (bifoldMap)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder as Builder
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (intercalate1)
import Data.Separated (Pesarated1)
import System.IO (BufferMode (BlockBuffering), hFlush, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Field
import Data.Sv.Record (Record (Record), Records, theRecords)
import Data.Sv.Sv (Sv (Sv), Header (Header), Separator)
import Text.Babel (Textual (toByteString, toByteStringBuilder), singleton)
import Text.Newline
import Text.Space (spaceToChar, Spaced (Spaced))
import Text.Quote

printNewline :: Newline -> Builder
printNewline n = toByteStringBuilder (newlineText n :: LB.ByteString)

printField :: Textual s => Field s -> Builder
printField f =
  case f of
    Unquoted s -> toByteStringBuilder s
    Quoted q ss ->
      let c = quoteToString q
          cc = c <> c
          s = bifoldMap (const cc) toByteStringBuilder ss
      in  fold [c, s, c]

printSpaced :: Textual s => Spaced (Field s) -> Builder
printSpaced (Spaced b t a) =
  let spc = foldMap (singleton . spaceToChar)
  in  fold [spc b, printField a, spc t]

printRecord :: Textual s => Separator -> Record s -> Builder
printRecord sep (Record fs) =
  intercalate1 (singleton sep) (fmap printSpaced fs)

printPesarated1 :: Textual s => Separator -> Pesarated1 Newline (Record s) -> Builder
printPesarated1 sep = bifoldMap printNewline (printRecord sep)

printRecords :: Textual s => Separator -> Records s -> Builder
printRecords sep = foldMap (printPesarated1 sep) . view theRecords

printHeader :: Textual s => Separator -> Header s -> Builder
printHeader sep (Header r n) = printRecord sep r <> printNewline n

printSv :: Textual s => Sv s -> Builder
printSv (Sv sep h rs e) =
  foldMap (printHeader sep) h <> printRecords sep rs <> foldMap printNewline e

writeSvToFile :: Textual s => FilePath -> Sv s -> IO ()
writeSvToFile fp sv = do
  let b = printSv sv
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  hPutBuilder h b
  hFlush h

displaySv :: Textual s => Sv s -> ByteString
displaySv = toByteString . printSv

displaySvLazy :: Textual s => Sv s -> LB.ByteString
displaySvLazy = toLazyByteString . printSv
