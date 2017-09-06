module Data.Csv.Field (
  Field (UnquotedF, QuotedF)
  , foldField
  , unspacedField
  , MonoField (MonoField, getField)
) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.Traversable   (Traversable (traverse))

import Text.Between       (Between, betwixt)
import Text.Escaped       (Escaped (SeparatedByEscapes))
import Text.Quote         (Quote, Quoted (Quoted))

data Field spc s1 s2 =
    UnquotedF s1
  | QuotedF (Between spc (Quoted s2))
  deriving (Eq, Ord, Show)

foldField :: (s1 -> b) -> (Between spc (Quoted s2) -> b) -> Field spc s1 s2 -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

unspacedField :: Monoid m => Quote -> s2 -> Field m s1 s2
unspacedField q s = QuotedF (betwixt mempty mempty (Quoted q (SeparatedByEscapes (pure s))))

instance Functor (Field spc s1) where
  fmap f = foldField UnquotedF (QuotedF . fmap (fmap f))

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


newtype MonoField spc s =
  MonoField { getField :: Field spc s s }
  deriving (Eq, Ord, Show)

instance Functor (MonoField spc) where
  fmap f = MonoField . bimap f f . getField

instance Foldable (MonoField spc) where
  foldMap f = bifoldMap (const mempty) f . getField

instance Traversable (MonoField spc) where
  traverse f = fmap MonoField . bitraverse f f . getField

instance Bifunctor MonoField where
  bimap _ g (MonoField (UnquotedF s)) = MonoField (UnquotedF (g s))
  bimap f g (MonoField (QuotedF b)) = MonoField (QuotedF (bimap f (fmap g) b))

instance Bifoldable MonoField where
  bifoldMap _ g (MonoField (UnquotedF s)) = g s
  bifoldMap f g (MonoField (QuotedF b)) = bifoldMap f (foldMap g) b

instance Bitraversable MonoField where
  bitraverse _ g (MonoField (UnquotedF s)) = (MonoField . UnquotedF) <$> g s
  bitraverse f g (MonoField (QuotedF b)) = (MonoField . QuotedF) <$> bitraverse f (traverse g) b

