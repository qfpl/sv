-- | Datatype for a single field or "cell" of a CSV file
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

import Text.Escaped       (Escaped (SeparatedByEscapes))
import Text.Space         (Spaced)
import Text.Quote         (Quote, Quoted (Quoted))

-- | A 'Field' is a single cell from a CSV document.
--   Its value is either surrounded by quotes ('QuotedF'), or it is
--   'UnquotedF'.
--
--   Any spacing around quotes is preserved.
data Field s1 s2 =
    UnquotedF s1
  | QuotedF (Spaced (Quoted s2))
  deriving (Eq, Ord, Show)

-- | The catamorphism for 'Field'
foldField :: (s1 -> b) -> (Spaced (Quoted s2) -> b) -> Field s1 s2 -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

-- | 'unspacedField' is a convenient constructor for quoted fields with
--   no spaces surrounding the quotes.
unspacedField :: Quote -> s2 -> Field s1 s2
unspacedField q s = QuotedF (pure (Quoted q (SeparatedByEscapes (pure s))))

instance Functor (Field s1) where
  fmap f = foldField UnquotedF (QuotedF . fmap (fmap f))

instance Foldable (Field s1) where
  foldMap f = foldField (const mempty) (foldMap (foldMap f))

instance Traversable (Field s1) where
  traverse f =
    foldField (pure . UnquotedF) (fmap QuotedF . traverse (traverse f))

instance Bifunctor Field where
  bimap f g = foldField (UnquotedF . f) (QuotedF . fmap (fmap g))

instance Bifoldable Field where
  bifoldMap f g = foldField f (foldMap (foldMap g))

instance Bitraversable Field where
  bitraverse f g =
    foldField (fmap UnquotedF . f) (fmap QuotedF . traverse (traverse g))


-- | Often a 'Field' will have its last two type variables the same.
--   This newtype gives useful instances to that case.
newtype MonoField s =
  MonoField { getField :: Field s s }
  deriving (Eq, Ord, Show)

instance Functor MonoField where
  fmap f = MonoField . bimap f f . getField

instance Foldable MonoField where
  foldMap f = bifoldMap (const mempty) f . getField

instance Traversable MonoField where
  traverse f = fmap MonoField . bitraverse f f . getField

