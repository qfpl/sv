{-|
Module      : Data.Sv.Decode
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module contains data structures, combinators, and primitives for
decoding a CSV into a list of your Haskell datatype.

A file can be read with 'parseDecodeFromFile'. If you already have the text
data in memory, it can be decoded with 'parseDecode'.
You will need a 'Decode' for your desired type.

A 'Decode' can be built using the primitives in this file. 'Decode'
is an 'Applicative' and an 'Data.Functor.Alt.Alt', allowing for composition
of these values with '<*>' and '<!>'

The primitive 'Decode's in this file which use 'ByteString' expect UTF-8
encoding. The Decode type has an instance of 'Data.Profunctor.Profunctor',
so you can 'lmap' or 'alterInput' to reencode on the way in.

This module is intended to be imported qualified like so

@
import qualified Data.Sv.Decode as D
@
-}

module Data.Sv.Decode (
  module Data.Sv.Decode.Core
) where

import Data.Sv.Decode.Core
