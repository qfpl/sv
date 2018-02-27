{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Data.Sv.Parse.Options
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Configuration to tell the parser what your file looks like.
-}

module Data.Sv.Parse.Options (
  ParseOptions (ParseOptions, _headedness, _endOnBlankLine, _parseSeparator, _encodeString)
, HasParseOptions (parseOptions, endOnBlankLine, encodeString)
, HasSeparator (..)
, HasHeadedness (..)
, defaultParseOptions
, orDefault
, defaultHeadedness
, defaultSeparator
) where

import Control.Lens (Lens, lens)
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Maybe (fromMaybe)

import Data.Sv.Syntax.Sv (HasSeparator (separator), HasHeadedness (headedness), Headedness (Headed), Separator, comma)

-- | An 'ParseOptions' informs the parser how to parse your file.
--
-- A default is provided as 'defaultParseOptions', seen below.
data ParseOptions s =
  ParseOptions {
  -- | Which separator does the file use? Usually this is 'comma', but it can
  -- also be 'pipe', or any other 'Char' ('Separator' = 'Char')
    _parseSeparator :: Separator

  -- | Whether there is a header row with column names or not.
  , _headedness :: Headedness

  -- | If a blank line is encountered, should the parse finish, or treat it as
  -- an empty row and continue?
  , _endOnBlankLine :: Bool

  -- | How should I turn a String into this type? This is a detail used by the parser.
  , _encodeString :: String -> s
  }

instance Functor ParseOptions where
  fmap f (ParseOptions s h e enc) = ParseOptions s h e (f . enc)

-- | Classy lenses for 'ParseOptions'
class (HasSeparator c, HasHeadedness c) => HasParseOptions c d s t | c -> s, d -> t, c t -> d, d s -> c where
  parseOptions :: Lens c d (ParseOptions s) (ParseOptions t)
  encodeString :: Lens c d (String -> s) (String -> t)
  endOnBlankLine :: c ~ d => Lens c d Bool Bool

instance HasParseOptions (ParseOptions s) (ParseOptions t) s t where
  parseOptions = id
  {-# INLINE parseOptions #-}
  encodeString = lens _encodeString (\c s -> c { _encodeString = s })
  endOnBlankLine = lens _endOnBlankLine (\c b -> c { _endOnBlankLine = b })

instance HasSeparator (ParseOptions s) where
  separator =
    lens _parseSeparator (\c s -> c { _parseSeparator = s })

instance HasHeadedness (ParseOptions s) where
  headedness =
    lens _headedness (\c h -> c { _headedness = h })

-- | 'defaultParseOptions' is used to parse a CSV file featuring a header row, using
-- Trifecta as the parsing library. It uses UTF-8 'ByteString's
defaultParseOptions :: ParseOptions ByteString
defaultParseOptions = ParseOptions defaultSeparator defaultHeadedness False fromString

-- | Use the 'defaultParseOptions' in the case of 'Nothing'
orDefault :: Maybe (ParseOptions ByteString) -> ParseOptions ByteString
orDefault = fromMaybe defaultParseOptions

-- | The default separator. Alias for 'comma'.
defaultSeparator :: Separator
defaultSeparator = comma

-- | The default is that a header is present.
defaultHeadedness :: Headedness
defaultHeadedness = Headed
