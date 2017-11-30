module Data.Csv.Decode.Error (
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
) where

import Data.Validation (AccValidation (AccSuccess, AccFailure), bindValidation)
import Text.Trifecta (Result (Success, Failure), _errDoc)

import Data.Csv.Decode.Type
import Data.Csv.Field
import Text.Babel (Textual, showT)

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

resultToDecodeError :: Textual e => Result a -> DecodeValidation e a
resultToDecodeError result = case result of
  Success a -> pure a
  Failure e -> badParse (showT (_errDoc e))
