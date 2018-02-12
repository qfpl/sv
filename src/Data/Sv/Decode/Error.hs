{-|
Module      : Data.Sv.Decode.Error
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

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
, validateEither
, validateEither'
, validateMay
, validateMay'
, validateTrifectaResult
) where

import Data.Validation (AccValidation (AccSuccess, AccFailure), bindValidation)
import Text.Trifecta (Result (Success, Failure), _errDoc)

import Data.Sv.Decode.Type
import Data.Sv.Syntax.Field
import Text.Babel (Textual, showT)

-- | Build a failing 'DecodeValidation'
decodeError :: DecodeError e -> DecodeValidation e a
decodeError = AccFailure . DecodeErrors . pure

-- | Fail with 'UnexpectedEndOfRow'
unexpectedEndOfRow :: DecodeValidation e a
unexpectedEndOfRow = decodeError UnexpectedEndOfRow

-- | Given the rest of the row, fail with 'ExpectedEndOfRow'
expectedEndOfRow :: [SpacedField e] -> DecodeValidation e a
expectedEndOfRow = decodeError . ExpectedEndOfRow

-- | Given the unknown value and the list of good canonical values,
-- fail with 'UnknownCanonicalValue'
unknownCanonicalValue :: e -> [(e, [e])] -> DecodeValidation e a
unknownCanonicalValue unknown valids =
  decodeError (UnknownCanonicalValue unknown valids)

-- | Fail with 'BadParse' with the given message
badParse :: e -> DecodeValidation e a
badParse = decodeError . BadParse

-- | Fail with 'BadDecode' with the given message
badDecode :: e -> DecodeValidation e a
badDecode = decodeError . BadDecode

-- | Build a 'DecodeValidation' from an 'Either'
validateEither :: Either (DecodeError e) a -> DecodeValidation e a
validateEither = validateEither' id

-- | Build a 'DecodeValidation' from an 'Either', given a function to build the error.
validateEither' :: (e -> DecodeError e') -> Either e a -> DecodeValidation e' a
validateEither' f = either (decodeError . f) pure

-- | Build a 'DecodeValidation' from a 'Maybe'
validateMay :: DecodeError e -> Maybe b -> DecodeValidation e b
validateMay e = maybe (decodeError e) pure

-- | Build a 'DecodeValidation' from a function that returns a 'Maybe'
validateMay' :: (a -> Maybe b) -> DecodeError e -> a -> DecodeValidation e b
validateMay' ab e a = validateMay e (ab a)

-- | Convert a Trifecta 'Result' to a DecodeValidation
validateTrifectaResult :: Textual e => (e -> DecodeError e) -> Result a -> DecodeValidation e a
validateTrifectaResult f result = case result of
  Success a -> pure a
  Failure e -> decodeError . f . showT . _errDoc $ e
