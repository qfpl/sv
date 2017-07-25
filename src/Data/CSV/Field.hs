module Data.CSV.Field where

import Data.CSV.Quote

data Field =
    Unquoted String
  | Quoted Quote String
  deriving (Eq, Ord, Show)

