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
-- 'defaultEncodeOptions'. This will determine whether your output file is
-- separated by commas or tabs, whether fields are quoted, and what kind of
-- newline you like.
data EncodeOptions =
  EncodeOptions {
    _encodeSeparator :: Separator
  , _spacingBefore :: Spaces
  , _spacingAfter :: Spaces
  , _quote :: Maybe Quote
  , _newline :: Newline
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
  {-# INLINE newline #-}
  {-# INLINE quote #-}
  {-# INLINE spacingAfter #-}
  {-# INLINE spacingBefore #-}
  {-# INLINE terminalNewline #-}
  encodeOptions = id
  newline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 y x6) (f x5)
  quote f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 y x5 x6) (f x4)
  spacingAfter f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 y x4 x5 x6) (f x3)
  spacingBefore f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 y x3 x4 x5 x6) (f x2)
  terminalNewline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 x5 y) (f x6)

-- | The default options for encoding.
--
-- The default is a CSV file with double-quotes, CRLF lines, no spacing around
-- fields, and no terminating newline.
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma mempty mempty (Just DoubleQuote) CRLF False
