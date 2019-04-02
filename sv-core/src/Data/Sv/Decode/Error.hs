{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

{-|
Module      : Data.Sv.Decode.Error
Copyright   : (C) CSIRO 2017-2019
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
, missingColumn
, missingHeader
, badConfig
, badParse
, badDecode

-- * Display
, displayErrors
, displayErrors'
, dieOnError
, dieOnError'

-- * Conversions
, validateEither
, validateEitherWith
, validateMaybe
, validateTrifectaResult

-- * Re-exports from @validation@
, bindValidation
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.String (IsString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Validation (Validation (Failure), bindValidation)
import Data.Vector (Vector)
import qualified Text.Trifecta.Result as Trifecta
import System.Exit (exitFailure)

import Data.Sv.Decode.Type

-- | Build a failing 'DecodeValidation'
decodeError :: DecodeError e -> DecodeValidation e a
decodeError = Failure . DecodeErrors . pure

-- | Fail with 'UnexpectedEndOfRow'
unexpectedEndOfRow :: DecodeValidation e a
unexpectedEndOfRow = decodeError UnexpectedEndOfRow

-- | Fail with 'ExpectedEndOfRow'. This takes the rest of the row, so that it
-- can be displayed to the user.
expectedEndOfRow :: Vector e -> DecodeValidation e a
expectedEndOfRow = decodeError . ExpectedEndOfRow

-- | Fail with 'UnknownCategoricalValue'.
-- It takes the unknown value and the list of good categorical values.
--
-- This mostly exists to be used by the 'Data.Sv.Decode.categorical' function.
unknownCategoricalValue :: e -> [[e]] -> DecodeValidation e a
unknownCategoricalValue unknown valids =
  decodeError (UnknownCategoricalValue unknown valids)

-- | Fail with 'MissingColumn' with the given column name. This is for when a
-- 'NameDecode' looks for a column that doesn't exist.
missingColumn :: e -> DecodeValidation e a
missingColumn = decodeError . MissingColumn

-- | Fail with 'MissingHeader'. This is for when the user asks for a header but
-- the input document is completely empty (that is, it has nothing that could be
-- considered a header).
missingHeader :: DecodeValidation e a
missingHeader = decodeError MissingHeader

-- | Fail with 'badConfig'. This is for when the user has asked for something
-- impossible, like to decode a CSV by column name while asserting there's no
-- header.
badConfig :: e -> DecodeValidation e a
badConfig = decodeError . BadConfig

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
validateEither = validateEitherWith id

-- | Build a 'DecodeValidation' from an 'Either', given a function to build the error.
validateEitherWith :: (e -> DecodeError e') -> Either e a -> DecodeValidation e' a
validateEitherWith f = either (decodeError . f) pure

-- | Build a 'DecodeValidation' from a 'Maybe'. You have to supply an error
-- to use in the 'Nothing' case
validateMaybe :: DecodeError e -> Maybe b -> DecodeValidation e b
validateMaybe e = maybe (decodeError e) pure

-- | Convert a "Text.Trifecta" 'Text.Trifecta.Result' to a 'DecodeValidation'
validateTrifectaResult :: (String -> DecodeError e) -> Trifecta.Result a -> DecodeValidation e a
validateTrifectaResult f =
  validateEitherWith f . trifectaResultToEither
    where
      trifectaResultToEither r = case r of
        Trifecta.Failure e -> Left . show . Trifecta._errDoc $ e
        Trifecta.Success a -> Right a

-- | Pretty print errors as a string. Each error is on its own line.
displayErrors :: DecodeErrors ByteString -> LT.Text
displayErrors = displayErrors' buildBytestring

-- | Pretty print errors as a string. Each error is on its own line.
--
-- This version lets you work with any 'String' type in your errors.
displayErrors' :: forall e. (e -> Builder) -> DecodeErrors e -> LT.Text
displayErrors' build (DecodeErrors errs) =
  let
    indent :: Builder -> Builder
    indent x = "  " <> x

    displayErr :: DecodeError e -> Builder
    displayErr e = indent $ case e of
      BadParse msg -> "Parsing the document failed. The error was: " <> build msg
      UnexpectedEndOfRow -> "Expected more fields, but the row ended."
      ExpectedEndOfRow extras ->
        "Expected fewer fields in the row. The extra fields contained: " <>
          commaSep (bquote <$> toList extras)
      UnknownCategoricalValue found required ->
        "Unknown categorical value found: " <> bquote found <> ". Expected one of: " <>
          (commaSep . fmap bquote . mconcat) required
      MissingColumn name -> "Could not find required column " <> bquote name
      MissingHeader -> "A header row was required, but one was not found."
      BadConfig msg -> "sv was misconfigured: " <> build msg
      BadDecode msg -> "Decoding a field failed: " <> build msg

    displayAndCount = count . displayErr
    Counted body c = foldMap1 displayAndCount errs
    spaceSep = mconcat . intersperse " "
    commaSep = mconcat . intersperse ", "
    quote s = "\"" <> s <> "\""
    bquote = quote . build
    pluralise n s =
      if n == 1
      then s
      else Builder.fromString (show n) <> " " <> s <> "s"
    heading = spaceSep ["The following", pluralise c "error", "occurred:"]
  in
    Builder.toLazyText $ heading <> "\n" <> body

-- | If the 'DecodeValidation' is a 'Failure', print a pretty error message
-- and call 'exitFailure'
dieOnError :: DecodeValidation ByteString a -> IO a
dieOnError = dieOnError' buildBytestring

-- | If the 'DecodeValidation' is a 'Failure', print a pretty error message
-- and call 'exitFailure'
--
-- This version lets you work with different String types.
dieOnError' :: (e -> Builder) -> DecodeValidation e a -> IO a
dieOnError' build e = case e of
  Failure errs -> do
    LT.putStrLn $ displayErrors' build errs
    exitFailure
  Success a -> pure a

---- internal

buildBytestring :: ByteString -> Builder
buildBytestring bs = case T.decodeUtf8' bs of
  Left  _ -> Builder.fromString $ Char8.unpack bs
  Right b -> Builder.fromText b

data Counted e = Counted e Integer

count :: e -> Counted e
count e = Counted e 1

instance (Semigroup e, IsString e) => Semigroup (Counted e) where
  Counted b c <> Counted b' c' =
    Counted (b <> "\n" <> b') (c+c')
