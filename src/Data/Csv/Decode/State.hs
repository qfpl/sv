module Data.Csv.Decode.State (
  DecodeState (..)
, decodeState
, runDecodeState
) where

import Control.Monad.State (state, runState)
import Data.Csv.Field (Field)
import Data.Csv.Decode.Type (DecodeState (..))

decodeState :: ([Field s] -> (a, [Field s])) -> DecodeState s a
decodeState = DecodeState . state

runDecodeState :: DecodeState s a -> [Field s] -> (a, [Field s])
runDecodeState = runState . getDecodeState
