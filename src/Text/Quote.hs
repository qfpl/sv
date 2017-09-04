module Text.Quote where

import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap), (<$>))
import Data.List.NonEmpty (NonEmpty ((:|)))
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
  , value :: Escaped a
  }
  deriving (Eq, Ord, Show)

instance Functor Quoted where
  fmap f (Quoted q a) = Quoted q (fmap f a)

instance Foldable Quoted where
  foldMap f = foldMap f . value

instance Traversable Quoted where
  traverse f (Quoted q a) = Quoted q <$> traverse f a

newtype Escaped a =
  SeparatedByEscapes (NonEmpty a)
  deriving (Eq, Ord, Show)

instance Functor Escaped where
  fmap f (SeparatedByEscapes xs) = SeparatedByEscapes (fmap f xs)

instance Foldable Escaped where
  foldMap f (SeparatedByEscapes xs) = foldMap f xs

instance Traversable Escaped where
  traverse f (SeparatedByEscapes xs) = SeparatedByEscapes <$> traverse f xs

noEscape :: a -> Escaped a
noEscape a = SeparatedByEscapes (a :| [])

