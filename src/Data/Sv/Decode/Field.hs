module Data.Sv.Decode.Field (
  FieldDecode (..)
, runFieldDecode
, (==<<)
, (>>==)
, fieldDecode
, fieldDecode_
, decodeMay
, decodeMay'
, promote
) where


import Control.Lens (view)
import Control.Monad.State (state)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (rmapC)
import Data.Validation (_AccValidation, bindValidation)

import Data.Sv.Field (Field, SpacedField, FieldContents, fieldContents)
import Data.Sv.Decode.Error
import Data.Sv.Decode.State (runDecodeState)
import Data.Sv.Decode.Type
import Data.Sv.Record (Record, _fields)
import Text.Babel (Textual (retext))
import Text.Space (Spaced (_value))

runFieldDecode :: FieldDecode e s a -> [SpacedField s] -> (DecodeValidation e a, [SpacedField s])
runFieldDecode = runDecodeState . getCompose . unwrapFieldDecode

(==<<) :: (a -> DecodeValidation e b) -> FieldDecode e s a -> FieldDecode e s b
(==<<) f (FieldDecode c) =
  FieldDecode (rmapC (`bindValidation` (view _AccValidation . f)) c)
infixr 1 ==<<

(>>==) :: FieldDecode e s a -> (a -> DecodeValidation e b) -> FieldDecode e s b
(>>==) = flip (==<<)
infixl 1 >>==

fieldDecode_ :: (Field s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode_ f = spacedFieldDecode (f . _value)

spacedFieldDecode :: (SpacedField s -> DecodeValidation e a) -> FieldDecode e s a
spacedFieldDecode f = FieldDecode . Compose . state $ \l ->
  case l of
    [] -> (unexpectedEndOfRow, [])
    (x:xs) -> (f x, xs)

fieldDecode :: FieldContents s => (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . fieldContents)

decodeMay :: (a -> Maybe b) -> DecodeError e -> a -> DecodeValidation e b
decodeMay ab e a = decodeMay' e (ab a)

decodeMay' :: DecodeError e -> Maybe b -> DecodeValidation e b
decodeMay' e = maybe (decodeError e) pure

-- | promotes a FieldDecode to work on a whole 'Record' at once
--
-- TODO probably needs a waaaay better name
promote :: (Textual e, Textual s) => FieldDecode e s a -> Record s -> DecodeValidation e a
promote a rs =
  case runFieldDecode a . toList . _fields $ rs of
    (v, []) -> v
    (v, xs@(_:_)) -> v *> expectedEndOfRow (fmap (fmap (fmap retext)) xs)
