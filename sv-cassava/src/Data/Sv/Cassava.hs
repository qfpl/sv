{-|
Module      : Data.Sv.Cassava
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module provides integration between sv and cassava.
It lets you plug cassava's (very fast!) parser into sv's decoding layer.

Our benchmarking indicates that parsing is very expensive,
while decoding is comparatively less performance-sensitive.
So if performance matters to you, and cassava's parser is sufficient for
your needs, you might as well use it!

sv's own parser is much slower than cassava's right now, but aims to have
better error messages and keep more information for you to work with, such as
spacing, quoting, and newline information.
-}

module Data.Sv.Cassava (
  parseDecodeFromCassava
, parseCassava
, decodeFromCassava
) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 as UTF8
import qualified Data.Csv as Cassava
import qualified Data.Csv.Parser as Cassava
import Data.Maybe (mapMaybe)
import Data.Sv
import qualified Data.Sv.Decode as D
import Data.Vector (Vector, (!?))
import Data.Vector.NonEmpty (NonEmptyVector (NonEmptyVector))
import qualified Data.Vector as V
import Text.Escape (Unescaped (Unescaped))
import Text.Space (unspaced)
import Text.Quote (Quote (DoubleQuote))

-- | Use an sv 'Decode' to decode from cassava's 'Cassava.Csv' type.
decodeFromCassava :: Decode' ByteString a -> Cassava.Csv -> DecodeValidation ByteString [a]
decodeFromCassava d =
  traverse (D.promote d) . fs2r . V.toList
    where
      fs2r :: [Vector Cassava.Field] -> [Record ByteString]
      fs2r = mapMaybe (fmap (Record . fmap (unspaced . Quoted DoubleQuote . Unescaped)) . vec2nev)
      vec2nev :: Vector b -> Maybe (NonEmptyVector b)
      vec2nev v = NonEmptyVector <$> v !? 0 <*> pure (V.drop 1 v)

-- | Parse a 'Cassava.Csv' from a 'ByteString' using cassava's parser
--
-- This returns its result in a 'DecodeValidation', so that it's compatible
-- with the rest of sv.
parseCassava :: Cassava.DecodeOptions -> ByteString -> DecodeValidation ByteString Cassava.Csv
parseCassava opts =
  D.validateEither' (BadParse . UTF8.fromString) . parseOnly (Cassava.csv opts)

-- | Parse a 'Cassava.Csv' from a 'ByteString' using cassava's parser, then
-- decode it using the given 'Decode'.
--
-- This has the benefit of letting you use cassava's parser, which is very fast,
-- with sv's decoding.
parseDecodeFromCassava :: Decode' ByteString a -> Headedness -> Cassava.DecodeOptions -> ByteString -> DecodeValidation ByteString [a]
parseDecodeFromCassava d h opts bs =
  (chompFirst h <$> parseCassava opts bs) `bindValidation` decodeFromCassava d
    where
      -- The csv returned from cassava's parser may include a header row.
      -- If it does, we want to skip that row.
      chompFirst Headed   = V.drop 1
      chompFirst Unheaded = id
