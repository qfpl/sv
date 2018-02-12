{-|
Module      : Data.Sv.Parser.Options
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Configuration to tell the parser what your file looks like.
-}

module Data.Sv.Parser.Options (
  ParseOptions (ParseOptions, _headedness, _separator, _parsingLib)
, HasParseOptions (parseOptions, endOnBlankLine)
, defaultParseOptions
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

-- | An 'ParseOptions' informs the parser how to parse your file.
--
-- A default is provided as 'defaultParseOptions', seen below.
data ParseOptions =
  ParseOptions {
  -- | Which separator does the file use? Usually this is 'comma', but it can
  -- also be 'pipe', or any other 'Char' ('Separator' = 'Char')
    _separator :: Separator

  -- | Whether there is a header row with column names or not.
  , _headedness :: Headedness

  -- | If a blank line is encountered, should the parse finish, or treat it as
  -- an empty row and continue?
  , _endOnBlankLine :: Bool

  -- | Which parsing library should be used? 'Trifecta' or 'Attoparsec'?
  , _parsingLib :: ParsingLib
  }

-- | Classy lenses for 'ParseOptions'
class (HasSeparator c, HasHeadedness c, HasParsingLib c) => HasParseOptions c where
  parseOptions :: Lens' c ParseOptions
  endOnBlankLine :: Lens' c Bool

instance HasParseOptions ParseOptions where
  parseOptions = id
  {-# INLINE parseOptions #-}
  endOnBlankLine =
    lens _endOnBlankLine (\c b -> c { _endOnBlankLine = b })

instance HasSeparator ParseOptions where
  separator =
    lens _separator (\c s -> c { _separator = s })

instance HasHeadedness ParseOptions where
  headedness =
    lens _headedness (\c h -> c { _headedness = h })

instance HasParsingLib ParseOptions where
  parsingLib =
    lens _parsingLib (\c p -> c { _parsingLib = p })

-- | 'defaultParseOptions' is used to parse a CSV file featuring a header row, using
-- Trifecta as the parsing library.
defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions defaultSeparator defaultHeadedness False defaultParsingLib

-- | Use the 'defaultParseOptions' in the case of 'Nothing'
orDefault :: Maybe ParseOptions -> ParseOptions
orDefault = fromMaybe defaultParseOptions

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
