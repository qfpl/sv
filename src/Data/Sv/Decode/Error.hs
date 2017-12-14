module Data.Sv.Decode.Error (
  DecodeError (..)
, DecodeErrors (..)
, DecodeValidation
, AccValidation (AccFailure, AccSuccess)
, bindValidation
, decodeError
, unexpectedEndOfRow
, expectedEndOfRow
, unknownCanonicalValue
, badParse
, badDecode
, resultToDecodeError
, eitherToDecodeError
, eitherToDecodeError'
) where

import Data.Validation (AccValidation (AccSuccess, AccFailure), bindValidation)
import Text.Trifecta (Result (Success, Failure), _errDoc)

import Data.Sv.Decode.Type
import Data.Sv.Field
import Text.Babel (Textual, retext, showT)

decodeError :: DecodeError e -> DecodeValidation e a
decodeError = AccFailure . DecodeErrors . pure

unexpectedEndOfRow :: DecodeValidation e a
unexpectedEndOfRow = decodeError UnexpectedEndOfRow

expectedEndOfRow :: [Field e] -> DecodeValidation e a
expectedEndOfRow = decodeError . ExpectedEndOfRow

unknownCanonicalValue :: e -> [(e, [e])] -> DecodeValidation e a
unknownCanonicalValue unknown valids = decodeError (UnknownCanonicalValue unknown valids)

badParse :: e -> DecodeValidation e a
badParse = decodeError . BadParse

badDecode :: e -> DecodeValidation e a
badDecode = decodeError . BadDecode

resultToDecodeError :: Textual e => (e -> DecodeError e) -> Result a -> DecodeValidation e a
resultToDecodeError f result = case result of
  Success a -> pure a
  Failure e -> decodeError . f . showT . _errDoc $ e

eitherToDecodeError :: (e -> DecodeError e') -> Either e a -> DecodeValidation e' a
eitherToDecodeError f = either (decodeError . f) pure

eitherToDecodeError' :: (Textual e, Textual e') => (e' -> DecodeError e'') -> Either e a -> DecodeValidation e'' a
eitherToDecodeError' f = either (decodeError . f . retext) pure
