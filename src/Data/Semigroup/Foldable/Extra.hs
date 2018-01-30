module Data.Semigroup.Foldable.Extra
  (
    toNonEmpty
  )
where

import Data.List.NonEmpty      (NonEmpty)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))

-- | Convert any 'Foldable1' to a 'NonEmpty' list. This is only here to
-- support versions of semigroupoids < 5.2.1
toNonEmpty :: Foldable1 f => f a -> NonEmpty a
toNonEmpty = foldMap1 pure
