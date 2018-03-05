{-|
Module      : Data.Sv.Encode.Options
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Options to configure encoding
-}

module Data.Sv.Encode.Options (
  EncodeOptions (EncodeOptions, _encodeSeparator, _spacingBefore, _spacingAfter, _quote, _newline, _terminalNewline)
, HasEncodeOptions (encodeOptions, spacingBefore, spacingAfter, quote, newline, terminalNewline)
, HasSeparator (..)
, defaultEncodeOptions
) where

import Control.Lens (Lens')

import Data.Sv.Syntax.Sv (Separator, HasSeparator (separator), comma)
import Text.Newline (Newline (CRLF))
import Text.Space (Spaces)
import Text.Quote (Quote (DoubleQuote))

-- | These are options to configure encoding. A default is provided as
-- 'defaultEncodeOptions'.
data EncodeOptions =
  EncodeOptions {
    -- | Are your values separated by commas, tabs, or something else? Default: comma
    _encodeSeparator :: Separator
    -- | Between a comma and the next value, would you like some spacing? Default: no spacing
  , _spacingBefore :: Spaces
    -- | Between a value and the next comma, would you like some spacing? Default: no spacing
  , _spacingAfter :: Spaces
    -- | Would you like quotes around your values? If so, double quotes or single? Deafult: Double quotes
  , _quote :: Maybe Quote
    -- | What kind of newline would you like? Default: CRLF
  , _newline :: Newline
    -- | Should the file be terminated with a newline? Default: No
  , _terminalNewline :: Bool
  }

-- | Classy lenses for 'EncodeOptions'
--
-- @
-- import Control.Lens
--
-- defaultEncodeOptinons & quote .~ Just DoubleQuote & newline .~ LF
-- @
class HasSeparator c => HasEncodeOptions c where
  encodeOptions :: Lens' c EncodeOptions
  newline :: Lens' c Newline
  {-# INLINE newline #-}
  quote :: Lens' c (Maybe Quote)
  {-# INLINE quote #-}
  spacingAfter :: Lens' c Spaces
  {-# INLINE spacingAfter #-}
  spacingBefore :: Lens' c Spaces
  {-# INLINE spacingBefore #-}
  terminalNewline :: Lens' c Bool
  {-# INLINE terminalNewline #-}
  newline = encodeOptions . newline
  quote = encodeOptions . quote
  spacingAfter = encodeOptions . spacingAfter
  spacingBefore = encodeOptions . spacingBefore
  terminalNewline = encodeOptions . terminalNewline

instance HasSeparator EncodeOptions where
  separator f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions y x2 x3 x4 x5 x6) (f x1)
  {-# INLINE separator #-}

instance HasEncodeOptions EncodeOptions where
  encodeOptions = id
  {-# INLINE encodeOptions #-}
  newline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 y x6) (f x5)
  {-# INLINE newline #-}
  quote f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 y x5 x6) (f x4)
  {-# INLINE quote #-}
  spacingAfter f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 y x4 x5 x6) (f x3)
  {-# INLINE spacingAfter #-}
  spacingBefore f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 y x3 x4 x5 x6) (f x2)
  {-# INLINE spacingBefore #-}
  terminalNewline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 x5 y) (f x6)
  {-# INLINE terminalNewline #-}

-- | The default options for encoding.
--
-- The default is a CSV file with double-quotes, CRLF lines, no spacing around
-- fields, and no terminating newline.
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma mempty mempty (Just DoubleQuote) CRLF False
