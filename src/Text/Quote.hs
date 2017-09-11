-- | A sum type for quote characters
module Text.Quote (
  Quote (SingleQuote, DoubleQuote)
  , quoteChar
  , quoteString
  , Quoted (Quoted, quote, value)
) where

import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap), (<$>))
import Data.String (IsString (fromString))
import Data.Traversable (Traversable (traverse))

import Text.Escaped       (Escaped)

-- | A sum type for quote characters. Either single or double quotes.
data Quote =
    SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

-- | Convert a Quote to the Char it represents.
quoteChar :: Quote -> Char
quoteChar q =
  case q of
    SingleQuote -> '\''
    DoubleQuote -> '"'
-- TODO replace this with a prism

-- | Convert a @Quote@ into a String.
-- Useful when concatenating strings
quoteString :: IsString s => Quote -> s
quoteString = fromString . pure . quoteChar
-- TODO probably replace this with a prism?

-- | A 'Quoted a' is a collection of 'a's separated by escapes given by 'quote'
data Quoted a =
  Quoted {
    quote :: Quote
  , value :: Escaped a
  }
  deriving (Eq, Ord, Show)

instance Functor Quoted where
  fmap f (Quoted q a) = Quoted q (fmap f a)

instance Foldable Quoted where
  foldMap f = foldMap f . value

instance Traversable Quoted where
  traverse f (Quoted q a) = Quoted q <$> traverse f a

