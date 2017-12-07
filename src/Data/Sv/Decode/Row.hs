module Data.Sv.Decode.Row (
  RowDecode (RowDecode, unwrapRowDecode)
, rowDecode
, runRowDecode
, row
, (<&>)
) where

import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Data.Sv.Record (Record, _fields)
import Data.Sv.Decode.Error (DecodeValidation, expectedEndOfRow)
import Data.Sv.Decode.Field (FieldDecode, runFieldDecode)
import Data.Sv.Decode.Type (RowDecode (RowDecode, unwrapRowDecode))
import Data.Foldable (toList)
import Text.Babel (Textual, retext)

rowDecode :: (Record s -> DecodeValidation e a) -> RowDecode e s a
rowDecode = RowDecode . ReaderT

runRowDecode :: RowDecode e s a -> Record s -> DecodeValidation e a
runRowDecode = runReaderT . unwrapRowDecode

row :: (Textual e, Textual s) => FieldDecode e s a -> RowDecode e s a
row a = rowDecode $ \rs ->
  case runFieldDecode a . toList . _fields $ rs of
    (v, []) -> v
    (v, xs@(_:_)) -> v *> expectedEndOfRow (fmap (fmap retext) xs)

(<&>) :: (Textual s, Textual e) => FieldDecode e s (a -> b) -> FieldDecode e s a -> RowDecode e s b
(<&>) ab a = row (ab <*> a)
infixl 4 <&>
