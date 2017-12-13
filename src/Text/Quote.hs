{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | A sum type for quote characters
module Text.Quote (
  Quote (SingleQuote, DoubleQuote)
  , AsQuote (_Quote, _SingleQuote, _DoubleQuote)
  , quoteChar
  , quoteText
  , quoteToString
  , Quoted (Quoted, _quote, _value)
  , HasQuoted (quoted, quote, value)
  , expand
) where

import Control.Lens (Lens', Prism', prism, prism', review)
import Data.Bifunctor (first)
import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap), (<$>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Traversable (Traversable (traverse))

import Data.Sv.Lens.Util (singletonList, singletonText)
import Text.Escaped (Escaped, Escaped')

-- | A sum type for quote characters. Either single or double quotes.
data Quote =
    SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show)

-- | Classy prisms for 'Quote'
class AsQuote r where
  _Quote :: Prism' r Quote
  _SingleQuote :: Prism' r ()
  _DoubleQuote :: Prism' r ()
  _SingleQuote = _Quote . _SingleQuote
  _DoubleQuote = _Quote . _DoubleQuote

instance AsQuote Quote where
  _Quote = id
  _SingleQuote = prism (const SingleQuote) $ \x -> case x of
    SingleQuote -> Right ()
    DoubleQuote -> Left x
  _DoubleQuote = prism (const DoubleQuote) $ \x -> case x of
    SingleQuote -> Left x
    DoubleQuote -> Right ()

instance AsQuote Char where
  _Quote = quoteChar

instance (a ~ Char) => AsQuote [a] where
  _Quote = singletonList . _Quote

instance AsQuote Text where
  _Quote = quoteText

-- | Convert a Quote to the Char it represents.
quoteChar :: Prism' Char Quote
quoteChar =
  prism'
    (\q -> case q of
      SingleQuote -> '\''
      DoubleQuote -> '"')
    (\c -> case c of
      '\'' -> Just SingleQuote
      '"'  -> Just DoubleQuote
      _    -> Nothing)

-- | Convert a 'Quote' into a String.
-- Useful when concatenating strings
quoteText :: Prism' Text Quote
quoteText = singletonText . quoteChar

-- | Convert a quote to a 'String'. Works for any stringy type, like 'Text'
-- or 'ByteString'.
quoteToString :: IsString a => Quote -> a
quoteToString = fromString . pure . review quoteChar

-- | A 'Quoted a' is a collection of 'a's separated by escapes given by 'quote'
data Quoted a =
  Quoted {
    _quote :: Quote
  , _value :: Escaped' a
  }
  deriving (Eq, Ord, Show)

-- | Classy lenses for 'Quoted'
class HasQuoted c a | c -> a where
  quoted :: Lens' c (Quoted a)
  quote :: Lens' c Quote
  {-# INLINE quote #-}
  value :: Lens' c (Escaped' a)
  {-# INLINE value #-}
  quote = quoted . quote
  value = quoted . value

instance HasQuoted (Quoted a) a where
  {-# INLINE quote #-}
  {-# INLINE value #-}
  quoted = id
  quote f (Quoted q v) = flip Quoted v <$> f q
  value f (Quoted q v) = Quoted q <$> f v

instance Functor Quoted where
  fmap f (Quoted q a) = Quoted q (fmap f a)

instance Foldable Quoted where
  foldMap f = foldMap f . _value

instance Traversable Quoted where
  traverse f (Quoted q a) = Quoted q <$> traverse f a

-- | Expands a Quoted, which is compact with all quotes the same, into a
-- WithEscapes Quote, which is often a useful representation, particularly
-- because of its instances of Bitraversable and friends.
expand :: Quoted a -> Escaped Quote a
expand (Quoted q v) = first (const q) v
-- TODO maybe make this a prism

