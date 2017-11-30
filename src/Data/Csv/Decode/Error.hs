{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Csv.Decode.Error where

import Data.List.NonEmpty
import Data.Semigroup
import Data.Validation
import Text.Trifecta (Result (Success, Failure), _errDoc)

import Data.Csv.Field
import Text.Babel (Textual, showT)

-- TODO eventually give this type a much better show
data DecodeError e =
  UnexpectedEndOfRow
  | ExpectedEndOfRow [Field e]
  | UnknownCanonicalValue e [(e, [e])]
  | BadParse e
  | BadDecode e
  deriving (Eq, Ord, Show)

newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup)

type DecodeValidation e = AccValidation (DecodeErrors e)

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
