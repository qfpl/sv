{-|
Module      : Data.Sv.Decode.State
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.State (
  DecodeState (..)
, decodeState
, runDecodeState
) where

import Control.Monad.State (state, runState)

import Data.Sv.Decode.Type (DecodeState (..))
import Data.Sv.Syntax.Field (SpacedField)

-- | Convenient helper to build a DecodeState
decodeState :: ([SpacedField s] -> (a, [SpacedField s])) -> DecodeState s a
decodeState = DecodeState . state

-- | Convenient function to run a DecodeState
runDecodeState :: DecodeState s a -> [SpacedField s] -> (a, [SpacedField s])
runDecodeState = runState . getDecodeState
