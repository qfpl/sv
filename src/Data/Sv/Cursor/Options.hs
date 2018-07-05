{-|
Module      : Data.Sv.Cursor.Options
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Cursor.Options (
  ParseOptions (..)
, defaultParseOptions
, defaultSeparator
, defaultHeadedness
) where

import Data.Sv.Cursor.Separator
import Data.Sv.Cursor.Headedness

-- | A 'ParseOptions' informs the parser how to parse your file.
--
-- A default is provided as 'defaultParseOptions', seen below.
data ParseOptions =
  ParseOptions {
  -- | Which separator does the file use? Usually this is 'comma', but it can
  -- also be 'pipe', or any other 'Word8' ('Separator' = 'Word8')
    _separator :: Separator

  -- | Whether there is a header row with column names or not.
  , _headedness :: Headedness
  }

instance HasHeadedness ParseOptions where
  headedness f (ParseOptions s h) = ParseOptions s <$> f h

instance HasSeparator ParseOptions where
  separator f (ParseOptions s h) = (\y -> ParseOptions y h) <$> f s

-- | Default parsing options.
--
-- The default is a comma separator, with a header at the top of the file.
defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions defaultSeparator defaultHeadedness

-- | The default separator is comma.
defaultSeparator :: Separator
defaultSeparator = comma

-- | The default is that a header is present.
defaultHeadedness :: Headedness
defaultHeadedness = Headed
