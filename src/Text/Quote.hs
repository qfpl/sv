{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Text.Quote
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

A sum type for quote characters
-}

module Text.Quote (
  Quote (SingleQuote, DoubleQuote)
  , AsQuote (_Quote, _SingleQuote, _DoubleQuote)
  , quoteChar
  , quoteToString
) where

import Control.Lens (Prism', prism, prism', review)
import Data.String (IsString (fromString))

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

-- | Convert a quote to a 'String'. Works for any stringy type, like 'Text'
-- or 'ByteString'.
quoteToString :: IsString a => Quote -> a
quoteToString = fromString . pure . review quoteChar
