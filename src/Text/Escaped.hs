-- | Chunks of text separated by escape sequences
module Text.Escaped (
  Escaped
  , Escape
  , noEscape
  , escaped
  , WithEscapes
) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid        (Monoid (mappend, mempty))
import Data.Semigroup     (Semigroup ((<>)))
import Data.Traversable   (Traversable (traverse))

-- | @WithEscapes e a@ is a list of characters separated by escape sequences.
--
-- 'a' can happily be 'Char', but you could just as easily use 'Text' to make
-- sure tricky characters work properly.
--
-- 'e' is the escape sequence, which should be represented by a bespoke sum
-- type rather than something like 'Text'.
newtype WithEscapes e a =
  WithEscapes { _escapeList :: [Either e a]}
  deriving (Eq, Ord, Show)

instance Semigroup (WithEscapes e a) where
  WithEscapes x <> WithEscapes y = WithEscapes (x <> y)

instance Monoid (WithEscapes e a) where
  mappend = (<>)
  mempty = WithEscapes mempty

instance Functor (WithEscapes e) where
  fmap = bimap id

instance Foldable (WithEscapes e) where
  foldMap = bifoldMap (const mempty)

instance Traversable (WithEscapes e) where
  traverse = bitraverse pure

instance Bifunctor WithEscapes where
  bimap f g = WithEscapes . fmap (bimap f g) . _escapeList

instance Bifoldable WithEscapes where
  bifoldMap f g = foldMap (bifoldMap f g) . _escapeList

instance Bitraversable WithEscapes where
  bitraverse f g = fmap WithEscapes . traverse (bitraverse f g) . _escapeList

-- | @Escaped@ is for text which only has one valid escape sequence
--
-- This representation only works when there is one escape
-- sequence, as it does not contain any information about which escape sequence
-- occured. The @WithEscapes@ type is used directly for that purpose.
type Escaped a =
  WithEscapes Escape a

-- | noEscape is a dramatically-named singleton constructor for Escaped.
noEscape :: a -> Escaped a
noEscape = WithEscapes . pure . pure

-- | Put escape sequences in between the elements of a non-empty list.
escaped :: NonEmpty a -> Escaped a
escaped as = case as of
  (a :|   []) -> noEscape a
  (a :| x:xs) -> WithEscapes $ Right a : Left Escape : _escapeList (escaped (x:|xs))

-- | A trivial type to be our sole escape sequence. The actual value of the escape is
-- determined externally.
data Escape =
  Escape
  deriving (Eq, Ord, Show)

instance Semigroup Escape where
  Escape <> Escape = Escape

instance Monoid Escape where
  mappend = (<>)
  mempty = Escape
