{-|
Module      : Data.Sv.Svfactor
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Svfactor (
  parseDecode
, parseDecode'
, decodeSvfactor
, ignoreSpaces
, ignoreField
, ignoreSpacedField
, treatAsQuoted
, treatAsQuotedUnspaced
, spacedField
, field
, contents
) where

import Control.Lens (view)
import Data.ByteString (ByteString)
import Data.Profunctor (Profunctor (lmap))
import Data.Svfactor.Parse
import Data.Svfactor.Syntax
import Data.Svfactor.Vector.NonEmpty (toVector)
import Data.Svfactor.Text.Space (spacedValue)
import Data.Svfactor.Text.Quote (Quote (DoubleQuote))
import qualified Data.Sv.Decode.Core as D
import Data.Validation

-- | Parse a 'ByteString' as an 'Sv' and then decode it using the given
-- decoder.
--
-- This version uses trifecta for parsing.
parseDecode :: D.Decode ByteString (SpacedField ByteString) a -> ParseOptions ByteString -> ByteString -> D.DecodeValidation ByteString [a]
parseDecode =
  parseDecode' trifecta

-- | Parse a 'ByteString' as an 'Sv' and then decode it using the given
-- decoder.
--
-- This version lets you control which parser is used.
parseDecode' :: SvParser s -> D.Decode s (SpacedField s) a -> ParseOptions s -> s -> D.DecodeValidation s [a]
parseDecode' svp dec opts = 
  flip bindValidation (decodeSvfactor dec) . D.validateEitherWith D.BadParse . parseSv' svp opts

-- | Decode from an 'Sv' using the given decoder
decodeSvfactor :: D.Decode s (SpacedField s) a -> Sv s -> D.DecodeValidation s [a]
decodeSvfactor dec =
  let dec' = D.promote' (view (spacedValue.fieldContents)) dec
  in  traverse dec' . fmap (toVector . view spacedFields) . recordList

-- | Promote a decoder that works on fields to one that works on fields with
-- optional surrounding spaces, by ignoring the spaces.
ignoreSpaces :: D.Decode e (Field s) a -> D.Decode e (SpacedField s) a
ignoreSpaces = lmap (view spacedValue)

-- | Promote a decoder to work on a field by ignoring the structure of the
-- field.
ignoreField :: D.Decode e s a -> D.Decode e (Field s) a
ignoreField  = lmap (view fieldContents)

-- | Promote a decoder to work on a spaced field by ignoring the spacing and
-- the field.
ignoreSpacedField :: D.Decode e s a -> D.Decode e (SpacedField s) a
ignoreSpacedField = ignoreSpaces . ignoreField

-- | Demote a decoder that works on fields to one that ignores the field
-- structure by always passing in a dummy set of double quotes.
treatAsQuoted :: D.Decode e (Field s) a -> D.Decode e s a
treatAsQuoted = lmap (unescapedField DoubleQuote)

-- | Demote a decoder that works on spaced fields to one that ignores the
-- spacing and field structure by always passing in a dummy set of double
-- quotes and empty spacing.
treatAsQuotedUnspaced :: D.Decode e (SpacedField s) a -> D.Decode e s a
treatAsQuotedUnspaced = treatAsQuoted . lmap pure

-- | Alias for 'D.contents'
spacedField :: D.Decode e (SpacedField s) (SpacedField s)
spacedField = D.contents

-- | Get the 'Field' from a 'SpacedField'
field :: D.Decode e (SpacedField s) (Field s)
field = fmap (view spacedValue) D.contents

-- | Get the contents from a 'SpacedField'
contents :: D.Decode e (SpacedField s) s
contents = fmap (view (spacedValue.fieldContents)) D.contents
