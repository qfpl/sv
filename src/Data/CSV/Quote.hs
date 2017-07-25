module Data.CSV.Quote where

data Quote =
    SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

quoteChar :: Quote -> Char
quoteChar q =
  case q of
    SingleQuote -> '\''
    DoubleQuote -> '"'

