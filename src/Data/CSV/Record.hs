module Data.CSV.Record where

import Data.CSV.Field (Field)

newtype Record =
  Record [Field]
  deriving (Eq, Ord, Show)

