module Data.Semigroup.Foldable.Extra
  (
    toNonEmpty
  )
where

import Data.List.NonEmpty      (NonEmpty)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))

toNonEmpty :: Foldable1 f => f a -> NonEmpty a
toNonEmpty = foldMap1 pure

