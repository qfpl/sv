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
  EncodeOptions (EncodeOptions, _encodeSeparator, _quoting, _newline, _terminalNewline)
, HasEncodeOptions (encodeOptions, quoting, newline, terminalNewline)
, HasSeparator (separator)
, Quoting (..)
, defaultEncodeOptions
) where

import Control.Lens (Lens')

import Data.Sv.Cursor.Newline (Newline, lf)
import Data.Sv.Cursor.Separator (Separator, HasSeparator (separator), comma)

-- | Should the output file have quotes around every value, or only when they
-- are required?
--
-- Beware the 'Never' constructor. It can construct malformed CSV files if
-- there are fields containing quotes, newlines, or separators. It is the
-- fastest option though, so you might like to use it if you're sure none
-- of your encoded data will include those characters.
data Quoting
  = Always
  | AsNeeded
  | Never

-- | These are options to configure encoding. A default is provided as
-- 'defaultEncodeOptions'.
data EncodeOptions =
  EncodeOptions {
    -- | Are your values separated by commas, tabs, or something else? Default: comma
    _encodeSeparator :: Separator
    -- | Would you like quotes around your values? If so, double quotes or single? Deafult: Double quotes
  , _quoting :: Quoting
    -- | What kind of newline would you like? Default: LF
  , _newline :: Newline
    -- | Should the file be terminated with a newline? Default: No
  , _terminalNewline :: Bool
  }

-- | Classy lenses for 'EncodeOptions'
--
-- @
-- import Control.Lens
--
-- defaultEncodeOptions & newline .~ crlf & quoting .~ Always
-- @
class HasSeparator c => HasEncodeOptions c where
  encodeOptions :: Lens' c EncodeOptions
  quoting :: Lens' c Quoting
  {-# INLINE quoting #-}
  newline :: Lens' c Newline
  {-# INLINE newline #-}
  terminalNewline :: Lens' c Bool
  {-# INLINE terminalNewline #-}
  newline = encodeOptions . newline
  quoting = encodeOptions . quoting
  terminalNewline = encodeOptions . terminalNewline

instance HasSeparator EncodeOptions where
  separator f (EncodeOptions x1 x2 x3 x4) =
    fmap (\ y -> EncodeOptions y x2 x3 x4) (f x1)
  {-# INLINE separator #-}

instance HasEncodeOptions EncodeOptions where
  encodeOptions = id
  {-# INLINE encodeOptions #-}
  quoting f (EncodeOptions x1 x2 x3 x4) =
    fmap (\ y -> EncodeOptions x1 y x3 x4) (f x2)
  {-# INLINE quoting #-}
  newline f (EncodeOptions x1 x2 x3 x4) =
    fmap (\ y -> EncodeOptions x1 x2 y x4) (f x3)
  {-# INLINE newline #-}
  terminalNewline f (EncodeOptions x1 x2 x3 x4) =
    fmap (EncodeOptions x1 x2 x3) (f x4)
  {-# INLINE terminalNewline #-}

-- | The default options for encoding.
--
-- The default is a CSV file with quotes when necessary, LF lines,
-- and no terminating newline.
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma AsNeeded lf False
