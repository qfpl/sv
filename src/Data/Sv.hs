{-|
Module      : Data.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module exports most of the other modules from the package. It is intended
to be imported unqualified, along with some qualified imports for the
"Data.Sv.Decode" and "Data.Sv.Encode" modules as needed.

@
import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
@
-}

module Data.Sv (
  -- * Decoding
    decode
  , parseDecode
  , parseDecodeFromFile
  , decodeMay
  , decodeEither
  , decodeEither'
  , (>>==)
  , (==<<)
  , module Data.Sv.Decode.Type
  , module Data.Sv.Decode.Error

  -- * Encoding
  , encode
  , encodeToFile
  , encodeToHandle
  , encodeBuilder
  , encodeRow
  , module Data.Sv.Encode.Type
  , module Data.Sv.Encode.Options

  -- * Cursor
  , module Data.Sv.Cursor

  -- * Structure
  , module Data.Sv.Structure.Headedness

  -- * Re-exports from contravariant and semigroupoids
  , Alt (..)
  , Contravariant (..)
  , Divisible (..)
  , divided
  , Decidable (..)
  , chosen
) where

import Data.Functor.Alt (Alt (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Divisible (..), divided, Decidable (..), chosen)

import Data.Sv.Cursor
import Data.Sv.Decode
import Data.Sv.Decode.Type
import Data.Sv.Decode.Error
import Data.Sv.Encode
import Data.Sv.Encode.Options
import Data.Sv.Encode.Type
import Data.Sv.Structure.Headedness