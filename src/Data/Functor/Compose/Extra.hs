{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Functor.Compose.Extra
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Functor.Compose.Extra (
  lmapC
, rmapC
) where

import Data.Functor.Compose (Compose (Compose))

-- | Run a natural transformation over the outer functor
lmapC :: (forall z. f z -> f' z) -> Compose f g a -> Compose f' g a
lmapC f (Compose fga) = Compose (f fga)

-- | Run a transformation (natural or otherwise) over the inner functor
rmapC :: Functor f => (g a -> g' a') -> Compose f g a -> Compose f g' a'
rmapC g (Compose fga) =
  Compose (fmap g fga)
