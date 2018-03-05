{-|
Module      : Data.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv (
  -- * Decoding
    decode
  , parseDecode
  , parseDecode'
  , parseDecodeFromFile
  , parseDecodeFromFile'
  , decodeMay
  , decodeEither
  , decodeEither'
  , (>>==)
  , (==<<)
  , module Data.Sv.Decode.Type
  , module Data.Sv.Decode.Error

  -- * Parsing
  , module Data.Sv.Parse

  -- * Printing
  , module Data.Sv.Print

  -- * Encoding
  , encode
  , encodeToFile
  , encodeToHandle
  , encodeBuilder
  , encodeRow
  , encodeSv
  , module Data.Sv.Encode.Type
  , module Data.Sv.Encode.Options

  -- * Core data types
  , module Data.Sv.Syntax

  -- * Re-exports from contravariant and semigroupoids
  , Alt ((<!>))
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible

) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import Data.Sv.Decode
import Data.Sv.Decode.Type
import Data.Sv.Decode.Error
import Data.Sv.Encode
import Data.Sv.Encode.Options
import Data.Sv.Encode.Type
import Data.Sv.Parse
import Data.Sv.Print
import Data.Sv.Syntax
