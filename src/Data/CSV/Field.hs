module Data.CSV.Field where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Trifunctor    (Trifunctor (trimap))
import Data.Trifoldable   (Trifoldable (trifoldMap))
import Data.Triversable   (Triversable (triverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.Traversable   (Traversable (traverse))

import Text.Between       (Between)
import Text.Quote         (Quoted)

data Field spc s1 s2 =
    UnquotedF s1
  | QuotedF (Between spc (Quoted s2))
  deriving (Eq, Ord, Show)

foldField :: (s1 -> b) -> (Between spc (Quoted s2) -> b) -> Field spc s1 s2 -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

instance Functor (Field spc s1) where
  fmap f = foldField UnquotedF (QuotedF . (fmap (fmap f)))

instance Foldable (Field spc s1) where
  foldMap f = foldField (const mempty) (foldMap (foldMap f))

instance Traversable (Field spc s1) where
  traverse f =
    foldField (pure . UnquotedF) (fmap QuotedF . traverse (traverse f))

instance Bifunctor (Field spc) where
  bimap f g = foldField (UnquotedF . f) (QuotedF . fmap (fmap g))

instance Bifoldable (Field spc) where
  bifoldMap f g = foldField f (foldMap (foldMap g))

instance Bitraversable (Field spc) where
  bitraverse f g =
    foldField (fmap UnquotedF . f) (fmap QuotedF . traverse (traverse g))

instance Trifunctor Field where
  trimap f g h = foldField (UnquotedF . g) (QuotedF . bimap f (fmap h))

instance Trifoldable Field where
  trifoldMap f g h = foldField g (bifoldMap f (foldMap h))

instance Triversable Field where
  triverse f g h = foldField (fmap UnquotedF . g) (fmap QuotedF . bitraverse f (traverse h))
