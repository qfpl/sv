-- | This module houses useful random lenses that didn't fit anywhere else
-- and don't warrant adding to lens itself.
module Data.Sv.Lens.Util (
  singletonText
, singletonList
) where

import Control.Lens (Prism', prism')
import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Data.Text as Text

-- | A prism for a singleton 'Text' value. This is different to just taking
-- the head, because it requires that the tail is empty.
singletonText :: Prism' Text Char
singletonText =
  prism'
    Text.singleton
    (Text.uncons >=> \(h,tl) -> if Text.null tl then Just h else Nothing)

-- | A prism for a singleton list value. This is different to just taking
-- the head, because it requires that the tail is empty.
singletonList :: Prism' [a] a
singletonList =
  prism' pure $ \s -> case s of
    [] -> Nothing
    (x:[]) -> Just x
    (_:_:_) -> Nothing
