module Data.Csv.Decode.Field (
  FieldDecode (..)
, runFieldDecode
, (==<<)
, (>>==)
, fieldDecode
, fieldDecode_
, decodeMay
, decodeMay'
) where


import Control.Lens (view)
import Control.Monad.State (state)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (rmapC)
import Data.Validation (_AccValidation, bindValidation)

import Data.Csv.Field (Field, FieldContents, fieldContents)
import Data.Csv.Decode.Error
import Data.Csv.Decode.State (runDecodeState)
import Data.Csv.Decode.Type

runFieldDecode :: FieldDecode e s a -> [Field s] -> (DecodeValidation e a, [Field s])
runFieldDecode = runDecodeState . getCompose . unwrapFieldDecode

(==<<) :: (a -> DecodeValidation e b) -> FieldDecode e s a -> FieldDecode e s b
(==<<) f (FieldDecode c) =
  FieldDecode (rmapC (`bindValidation` (view _AccValidation . f)) c)
infixr 1 ==<<

(>>==) :: FieldDecode e s a -> (a -> DecodeValidation e b) -> FieldDecode e s b
(>>==) = flip (==<<)
infixl 1 >>==

fieldDecode_ :: (Field s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode_ f = FieldDecode . Compose . state $ \l ->
  case l of
    [] -> (unexpectedEndOfRow, [])
    (x:xs) -> (f x, xs)

fieldDecode :: FieldContents s => (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . fieldContents)

decodeMay :: (a -> Maybe b) -> DecodeError e -> a -> DecodeValidation e b
decodeMay ab e a = decodeMay' e (ab a)

decodeMay' :: DecodeError e -> Maybe b -> DecodeValidation e b
decodeMay' e = maybe (decodeError e) pure
