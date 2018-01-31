{-|
Module      : Data.Semigroup.Foldable.Extra
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

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
