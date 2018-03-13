{-|
Module      : Data.Sv.Cassava
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module provides integration between sv and cassava. In particular,
it lets you plug cassava's blazingly fast parser into sv's decoding layer.
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

decodeFromCassava :: Decode' ByteString a -> Cassava.Csv -> DecodeValidation ByteString [a]
decodeFromCassava d =
  traverse (D.promote d) . fs2r . V.toList
    where
      fs2r :: [Vector Cassava.Field] -> [Record ByteString]
      fs2r = mapMaybe (fmap (Record . (fmap (unspaced . Quoted DoubleQuote . Unescaped))) . vec2nev)
      vec2nev :: Vector b -> Maybe (NonEmptyVector b)
      vec2nev v = NonEmptyVector <$> v !? 0 <*> pure (V.drop 1 v)

parseCassava :: Cassava.DecodeOptions -> ByteString -> DecodeValidation ByteString Cassava.Csv
parseCassava opts =
  D.validateEither' (BadParse . UTF8.fromString) . parseOnly (Cassava.csv opts)

parseDecodeFromCassava :: Decode' ByteString a -> Headedness -> Cassava.DecodeOptions -> ByteString -> DecodeValidation ByteString [a]
parseDecodeFromCassava d h opts bs =
  (chompFirst h <$> parseCassava opts bs) `bindValidation` decodeFromCassava d
    where
      chompFirst Headed   = V.drop 1
      chompFirst Unheaded = id
