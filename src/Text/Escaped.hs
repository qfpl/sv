module Text.Escaped (
  Escaped (SeparatedByEscapes)
  , noEscape
) where

import Data.Foldable      (Foldable (foldMap))
import Data.Traversable   (Traversable (traverse))
import Data.List.NonEmpty (NonEmpty ((:|)))

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
