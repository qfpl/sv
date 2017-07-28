module Text.Quote where

import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap))
import Data.Traversable (Traversable (traverse))

data Quote =
    SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

quoteChar :: Quote -> Char
quoteChar q =
  case q of
    SingleQuote -> '\''
    DoubleQuote -> '"'

data Quoted a =
  Quoted {
    quote :: Quote
  , value :: a
  }
  deriving (Eq, Ord, Show)

instance Functor Quoted where
  fmap f (Quoted q a) = Quoted q (f a)

instance Foldable Quoted where
  foldMap f = f . value

instance Traversable Quoted where
  traverse f (Quoted q a) = fmap (Quoted q) (f a)

