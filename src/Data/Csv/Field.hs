{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Datatype for a single field or "cell" of a CSV file
module Data.Csv.Field (
  Field' (UnquotedF, QuotedF)
  -- Optics
  , AsField (_Field, _UnquotedF, _QuotedF)
  , mono
  -- Functions
  , foldField
  , unspacedField
  , MonoField
  , downmix
  , upmix
) where

import Control.Lens        (Iso, iso, Prism', prism)
import Data.Bifoldable     (Bifoldable (bifoldMap))
import Data.Bifunctor      (Bifunctor (bimap))
import Data.Bifunctor.Join (Join (Join), runJoin)
import Data.Bitraversable  (Bitraversable (bitraverse))
import Data.Foldable       (Foldable (foldMap))
import Data.Functor        (Functor (fmap))
import Data.Traversable    (Traversable (traverse))

import Text.Escaped        (noEscape)
import Text.Space          (Spaced)
import Text.Quote          (Quote, Quoted (Quoted))

-- | A @Field'@ is a single cell from a CSV document.
--   Its value is either surrounded by quotes (@QuotedF@), or it is
--   @UnquotedF@.
--
--   Any spacing around quotes is preserved.
data Field' s1 s2 =
    UnquotedF s1
  | QuotedF (Spaced (Quoted s2))
  deriving (Eq, Ord, Show)

class AsField r s1 s2 | r -> s1 s2 where
  _Field :: Prism' r (Field' s1 s2)
  _UnquotedF :: Prism' r s1
  _QuotedF :: Prism' r (Spaced (Quoted s2))
  _UnquotedF = _Field . _UnquotedF
  _QuotedF = _Field . _QuotedF

instance AsField (Field' s1 s2) s1 s2 where
  _Field = id
  _UnquotedF =
    prism UnquotedF $ \x -> case x of
      UnquotedF y -> Right y
      QuotedF _ -> Left x
  _QuotedF =
    prism QuotedF $ \x -> case x of
      QuotedF y -> Right y
      UnquotedF _ -> Left x

-- | The catamorphism for @Field'@
foldField :: (s1 -> b) -> (Spaced (Quoted s2) -> b) -> Field' s1 s2 -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

-- | 'unspacedField' is a convenient constructor for quoted fields with
--   no spaces surrounding the quotes.
unspacedField :: Quote -> s2 -> Field' s1 s2
unspacedField q s = QuotedF (pure (Quoted q (noEscape s)))

instance Functor (Field' s1) where
  fmap f = foldField UnquotedF (QuotedF . fmap (fmap f))

instance Foldable (Field' s1) where
  foldMap f = foldField (const mempty) (foldMap (foldMap f))

instance Traversable (Field' s1) where
  traverse f =
    foldField (pure . UnquotedF) (fmap QuotedF . traverse (traverse f))

instance Bifunctor Field' where
  bimap f g = foldField (UnquotedF . f) (QuotedF . fmap (fmap g))

instance Bifoldable Field' where
  bifoldMap f g = foldField f (foldMap (foldMap g))

instance Bitraversable Field' where
  bitraverse f g =
    foldField (fmap UnquotedF . f) (fmap QuotedF . traverse (traverse g))

-- | Often a 'Field' will have its last two type variables the same.
--   This newtype gives useful instances to that case.
type MonoField s = Join Field' s

-- | @downmix@ turns a Field into a @MonoField@
downmix :: Field' s s -> MonoField s
downmix = Join

-- | @upmix@ turns a @MonoField@ back into a regular field.
upmix :: MonoField s -> Field' s s
upmix = runJoin

mono :: Iso (Field' s s) (Field' t t) (MonoField s) (MonoField t)
mono = iso downmix upmix

