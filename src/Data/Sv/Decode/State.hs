module Data.Sv.Decode.State (
  DecodeState (..)
, decodeState
, runDecodeState
) where

import Control.Monad.State (state, runState)
import Data.Sv.Field (SpacedField)
import Data.Sv.Decode.Type (DecodeState (..))

decodeState :: ([SpacedField s] -> (a, [SpacedField s])) -> DecodeState s a
decodeState = DecodeState . state

runDecodeState :: DecodeState s a -> [SpacedField s] -> (a, [SpacedField s])
runDecodeState = runState . getDecodeState
