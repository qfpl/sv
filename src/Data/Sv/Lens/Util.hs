module Data.Sv.Lens.Util (
  singletonText
, singletonList
) where

import Control.Lens (Prism', prism')
import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Data.Text as Text

singletonText :: Prism' Text Char
singletonText =
  prism'
    Text.singleton
    (Text.uncons >=> \(h,tl) -> if Text.null tl then Just h else Nothing)

singletonList :: Prism' [a] a
singletonList =
  prism' pure $ \s -> case s of
    [] -> Nothing
    (x:[]) -> Just x
    (_:_:_) -> Nothing
