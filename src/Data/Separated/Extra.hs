module Data.Separated.Extra where

import Control.Applicative ((<$>))
import Data.List.NonEmpty  (NonEmpty ((:|)), nonEmpty)
import Data.Separated      (Pesarated1 (Pesarated1), Separated1 (Separated1), sprinkle)

skrinple :: s -> NonEmpty a -> Pesarated1 s a
skrinple s (a:|as) =
  Pesarated1 (Separated1 a (sprinkle s as))

skrinpleMay :: s -> [a] -> Maybe (Pesarated1 s a)
skrinpleMay s as =
  skrinple s <$> nonEmpty as
