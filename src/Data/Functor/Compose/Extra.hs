{-# LANGUAGE RankNTypes #-}

module Data.Functor.Compose.Extra (
  lmapC
, rmapC
) where

import Data.Functor.Compose (Compose (Compose))

lmapC :: (forall z. f z -> f' z) -> Compose f g a -> Compose f' g a
lmapC f (Compose fga) = Compose (f fga)

rmapC :: Functor f => (g a -> g' a') -> Compose f g a -> Compose f g' a'
rmapC g (Compose fga) =
  Compose (fmap g fga)
