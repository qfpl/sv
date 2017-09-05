module Text.Quote (
  Quote (SingleQuote, DoubleQuote)
  , quoteChar
  , quoteString
  , Quoted (Quoted, quote, value)
) where

import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap), (<$>))
import Data.String (IsString (fromString))
import Data.Traversable (Traversable (traverse))

import Text.Escaped       (Escaped)

data Quote =
    SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

quoteChar :: Quote -> Char
quoteChar q =
  case q of
    SingleQuote -> '\''
    DoubleQuote -> '"'

quoteString :: IsString s => Quote -> s
quoteString = fromString . pure . quoteChar

data Quoted a =
  Quoted {
    quote :: Quote
  , value :: Escaped a
  }
  deriving (Eq, Ord, Show)

instance Functor Quoted where
  fmap f (Quoted q a) = Quoted q (fmap f a)

instance Foldable Quoted where
  foldMap f = foldMap f . value

instance Traversable Quoted where
  traverse f (Quoted q a) = Quoted q <$> traverse f a
