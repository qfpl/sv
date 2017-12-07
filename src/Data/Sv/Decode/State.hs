module Data.Sv.Decode.State (
  DecodeState (..)
, decodeState
, runDecodeState
) where

import Control.Monad.State (state, runState)
import Data.Sv.Field (Field)
import Data.Sv.Decode.Type (DecodeState (..))

decodeState :: ([Field s] -> (a, [Field s])) -> DecodeState s a
decodeState = DecodeState . state

runDecodeState :: DecodeState s a -> [Field s] -> (a, [Field s])
runDecodeState = runState . getDecodeState
