module Data.CSV.Field where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.Traversable   (Traversable (traverse))

import Text.Between       (Between)
import Text.Quote         (Quoted)

data Field spc str =
    UnquotedF str
  | QuotedF (Between spc (Quoted str))
  deriving (Eq, Ord, Show)

foldField :: (str -> b) -> (Between spc (Quoted str) -> b) -> Field spc str -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

instance Functor (Field spc) where
  fmap f = foldField (UnquotedF . f) (QuotedF . (fmap (fmap f)))

instance Foldable (Field spc) where
  foldMap f = foldField f (foldMap (foldMap f))

instance Traversable (Field spc) where
  traverse f =
    foldField (fmap UnquotedF . f) (fmap QuotedF . traverse (traverse f))

instance Bifunctor Field where
  bimap f g = foldField (UnquotedF . g) (QuotedF . bimap f (fmap g))

instance Bifoldable Field where
  bifoldMap f g = foldField g (bifoldMap f (foldMap g))

instance Bitraversable Field where
  bitraverse f g =
    foldField (fmap UnquotedF . g) (fmap QuotedF . bitraverse f (traverse g))

