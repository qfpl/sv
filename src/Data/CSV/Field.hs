module Data.CSV.Field where

data Field =
    Unquoted String
  -- TODO Maybe don't use Char here?
  | Quoted Char String
  deriving (Eq, Ord, Show)

