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

-- * Convenience constructors
, decodeError
, unexpectedEndOfRow
, expectedEndOfRow
, unknownCategoricalValue
, badParse
, badDecode

-- * Conversions
, validateEither
, validateEither'
, validateMay
, validateMay'
, trifectaResultToEither
, validateTrifectaResult

-- * Re-exports from @validation@
, bindValidation
) where

import Data.Validation (Validation (Failure), bindValidation)
import Data.Vector (Vector)
import qualified Text.Trifecta as Trifecta (Result (Success, Failure), _errDoc)

import Data.Sv.Decode.Type
import Data.Sv.Syntax.Field

-- | Build a failing 'DecodeValidation'
decodeError :: DecodeError e -> DecodeValidation e a
decodeError = Failure . DecodeErrors . pure

-- | Fail with 'UnexpectedEndOfRow'
unexpectedEndOfRow :: DecodeValidation e a
unexpectedEndOfRow = decodeError UnexpectedEndOfRow

-- | Fail with 'ExpectedEndOfRow'. This takes the rest of the row, so that it
-- can be displayed to the user.
expectedEndOfRow :: Vector (SpacedField e) -> DecodeValidation e a
expectedEndOfRow = decodeError . ExpectedEndOfRow

-- | Fail with 'UnknownCategoricalValue'.
-- It takes the unknown value and the list of good categorical values.
--
-- This mostly exists to be used by the 'Data.Sv.Decode.categorical' function.
unknownCategoricalValue :: e -> [[e]] -> DecodeValidation e a
unknownCategoricalValue unknown valids =
  decodeError (UnknownCategoricalValue unknown valids)

-- | Fail with 'BadParse' with the given message. This is for when the parse
-- step fails, and decoding does not even begin.
badParse :: e -> DecodeValidation e a
badParse = decodeError . BadParse

-- | Fail with 'BadDecode' with the given message. This is something of a
-- generic error for when decoding a field goes wrong.
badDecode :: e -> DecodeValidation e a
badDecode = decodeError . BadDecode

-- | Build a 'DecodeValidation' from an 'Either'
validateEither :: Either (DecodeError e) a -> DecodeValidation e a
validateEither = validateEither' id

-- | Build a 'DecodeValidation' from an 'Either', given a function to build the error.
validateEither' :: (e -> DecodeError e') -> Either e a -> DecodeValidation e' a
validateEither' f = either (decodeError . f) pure

-- | Build a 'DecodeValidation' from a 'Maybe'. You have to supply an error
-- to use in the 'Nothing' case
validateMay :: DecodeError e -> Maybe b -> DecodeValidation e b
validateMay e = maybe (decodeError e) pure

-- | Build a 'DecodeValidation' from a function that returns a 'Maybe'
-- You have to supply an error to use in the 'Nothing' case
validateMay' :: (a -> Maybe b) -> DecodeError e -> a -> DecodeValidation e b
validateMay' ab e a = validateMay e (ab a)

-- | Helper to convert "Text.Trifecta" 'Text.Trifecta.Result' to 'Either'.
trifectaResultToEither :: Trifecta.Result a -> Either String a
trifectaResultToEither result = case result of
  Trifecta.Success a -> Right a
  Trifecta.Failure e -> Left . show . Trifecta._errDoc $ e

-- | Convert a "Text.Trifecta" 'Text.Trifecta.Result' to a 'DecodeValidation'
validateTrifectaResult :: (String -> DecodeError e) -> Trifecta.Result a -> DecodeValidation e a
validateTrifectaResult f = validateEither' f . trifectaResultToEither

