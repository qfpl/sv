{-|
Module      : Data.Sv.Config
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Configuration to tell the parser what your file looks like.
-}

module Data.Sv.Config (
  SvConfig (SvConfig, _headedness, _separator, _parsingLib)
, HasSvConfig (svConfig)
, defaultConfig
, orDefault
, Separator
, HasSeparator (separator)
, defaultSeparator
, comma
, pipe
, tab
, Headedness (Unheaded, Headed)
, HasHeadedness (headedness)
, defaultHeadedness
, ParsingLib (Trifecta, Attoparsec)
, HasParsingLib (parsingLib)
, defaultParsingLib
) where

import Control.Lens (Lens', lens)
import Data.Maybe (fromMaybe)

-- | An 'SvConfig' informs the parser how to parse your file.
--
-- This includes the separator, whether or not a header row is present, and
-- allows the user to choose which parsing library to use, between Trifecta
-- and Attoparsec.
--
-- A default is provided as 'defaultConfig', seen below.
data SvConfig =
  SvConfig {
    _separator :: Separator
  , _headedness :: Headedness
  , _parsingLib :: ParsingLib
  }

-- | Classy lenses for 'SvConfig'
class (HasSeparator c, HasHeadedness c, HasParsingLib c) => HasSvConfig c where
  svConfig :: Lens' c SvConfig

instance HasSvConfig SvConfig where
  svConfig = id
  {-# INLINE svConfig #-}

instance HasSeparator SvConfig where
  separator =
    lens _separator (\c s -> c { _separator = s })

instance HasHeadedness SvConfig where
  headedness =
    lens _headedness (\c h -> c { _headedness = h })

instance HasParsingLib SvConfig where
  parsingLib =
    lens _parsingLib (\c p -> c { _parsingLib = p })

-- | 'defultConfig' is used to parse a CSV file featuring a header row, using
-- Trifecta as the parsing library.
defaultConfig :: SvConfig
defaultConfig = SvConfig defaultSeparator defaultHeadedness defaultParsingLib

-- | Use the 'defaultConfig' in the case of 'Nothing'
orDefault :: Maybe SvConfig -> SvConfig
orDefault = fromMaybe defaultConfig

-- | Does the 'Sv' have a 'Header' or not? A header is a row at the beginning
-- of a file which contains the string names of each of the columns.
--
-- If a header is present, it must not be decoded with the rest of the data.
data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

-- | Classy lens for 'Headedness'
class HasHeadedness c where
  headedness :: Lens' c Headedness

instance HasHeadedness Headedness where
  headedness = id

-- | The default is that a header is present.
defaultHeadedness :: Headedness
defaultHeadedness = Headed

-- | By what are your values separated? The answer is often 'comma', but not always.
--
-- A 'Separator' is just a 'Char'. It could be a sum type instead, since it
-- will usually be comma or pipe, but our preference has been to be open here
-- so that you can use whatever you'd like. There are test cases, for example,
-- ensuring that you're free to use null-byte separated values if you so desire.
type Separator = Char

-- | Classy lens for 'Separator'
class HasSeparator c where
  separator :: Lens' c Separator

instance HasSeparator Char where
  separator = id
  {-# INLINE separator #-}

-- | The default separator. Alias for 'comma'.
defaultSeparator :: Separator
defaultSeparator = comma

-- | The venerable comma separator. Used for CSV documents.
comma :: Separator
comma = ','

-- | The pipe separator. Used for PSV documents.
pipe :: Separator
pipe = '|'

-- | Tab is a separator too - why not?
tab :: Separator
tab = '\t'

-- | Which parsing library should be used to parse the document?
--
-- The parser is written in terms of the @parsers@ library, meaning it can be
-- instantiated to several different parsing libraries. By default, we use
-- 'Trifecta', because its error messages are so helpful. 'Attoparsec' might
-- be faster, but we haven't benchmarked our parser yet, so we don't know.
--
-- It is worth noting that Trifecta assumes UTF-8 encoding of the input data.
-- UTF-8 is backwards-compatible with 7-bit ASCII, so this will work for many
-- documents. However, not all documents are ASCII or UTF-8. For example, our
-- @test/species.csv@ test file is Windows-1252, which is a non-ISO extension
-- of latin1 8-bit ASCII. For documents encoded as Windows-1252, Trifecta's
-- assumption is invalid and parse errors result.
-- 'Attoparsec' works fine for this character encoding.
data ParsingLib =
  Trifecta | Attoparsec
  deriving (Eq, Ord, Show)

-- | Class lens for 'ParsingLib'
class HasParsingLib c  where
  parsingLib :: Lens' c ParsingLib

instance HasParsingLib ParsingLib where
  parsingLib = id
  {-# INLINE parsingLib #-}

-- | The default parsing library is 'Trifecta'.
defaultParsingLib :: ParsingLib
defaultParsingLib = Trifecta
