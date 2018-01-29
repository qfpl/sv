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
) where

import Control.Lens (Prism', prism, prism', review)
import Data.String (IsString (fromString))
import Data.Text (Text)

import Data.Sv.Lens.Util (singletonList, singletonText)

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
