{-|
Module      : Data.Sv.Parse.Internal
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module contains internal implementation details of sv's parser.
As the Internal name suggests, this file is exempt from the PVP. Depend
on this module at your own risk!
-}

module Data.Sv.Parse.Internal (
  separatedValues
  , header
  , field
  , singleQuotedField
  , doubleQuotedField
  , unquotedField
  , spaced
  , spacedField
  , record
  , records
  , ending
) where

import Control.Applicative (Alternative ((<|>), empty), optional)
import Control.Lens (review, view)
import Data.CharSet (CharSet, (\\))
import qualified Data.CharSet as CharSet (fromList, insert, singleton)
import Data.Functor (($>), (<$>), void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Vector as V
import Text.Parser.Char (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import Text.Parser.Combinators (between, choice, eof, many, notFollowedBy, sepEndBy, try)

import Data.Sv.Syntax.Sv (Sv (Sv), Header, mkHeader, noHeader, Headedness (Unheaded, Headed), headedness, Separator)
import Data.Sv.Syntax.Field (Field (Unquoted, Quoted))
import Data.Sv.Syntax.Record (Record (Record), Records (Records, EmptyRecords))
import Data.Sv.Parse.Options (ParseOptions, separator, endOnBlankLine, encodeString)
import Data.Vector.NonEmpty as V
import Text.Escape (Unescaped (Unescaped))
import Text.Newline (Newline (CR, CRLF, LF))
import Text.Space (HorizontalSpace (Space, Tab), Spaces, Spaced, betwixt)
import Text.Quote (Quote (SingleQuote, DoubleQuote), quoteChar)

-- | This function is in newer versions of the parsers package, but in
-- order to maintain compatibility with older versions I've left it here.
sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

-- | Parse a field surrounded by single quotes
singleQuotedField :: CharParsing m => (String -> s) -> m (Field s)
singleQuotedField = quotedField SingleQuote

-- | Parse a field surrounded by double quotes
doubleQuotedField :: CharParsing m => (String -> s) -> m (Field s)
doubleQuotedField = quotedField DoubleQuote

-- | Given a quote, parse its escaped form (which is it repeated twice)
escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

quotedField :: CharParsing m => Quote -> (String -> s) -> m (Field s)
quotedField quote str =
  let q = review quoteChar quote
      c = char q
      cc = escapeQuote quote
  in  Quoted quote . Unescaped . str <$> between c c (many (cc <|> notChar q))

-- | Parse a field that is not surrounded by quotes
unquotedField :: CharParsing m => Separator -> (String -> s) -> m (Field s)
unquotedField sep str =
  let spaceSet = CharSet.fromList " \t" \\ CharSet.singleton sep
      oneSpace = oneOfSet spaceSet
      nonSpaceFieldChar = noneOfSet (newlineOr sep <> spaceSet)
      terminalWhitespace = many oneSpace *> fieldEnder
      fieldEnder = void (oneOfSet (newlineOr sep)) <|> eof
  in  Unquoted . str <$>
    many (
      nonSpaceFieldChar
      <|> (notFollowedBy (try terminalWhitespace)) *> oneSpace
    )

-- | Parse a field, be it quoted or unquoted
field :: CharParsing m => Separator -> (String -> s) -> m (Field s)
field sep str =
  choice [
    singleQuotedField str
  , doubleQuotedField str
  , unquotedField sep str
  ]

-- | Parse a field with its surrounding spacing
spacedField :: CharParsing m => Separator -> (String -> s) -> m (Spaced (Field s))
spacedField sep str = spaced sep (field sep str)

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

newline :: CharParsing m => m Newline
newline =
  CRLF <$ try (string "\r\n")
    <|> CR <$ char '\r'
    <|> LF <$ char '\n'

space :: CharParsing m => Separator -> m HorizontalSpace
space sep =
  let removeIfSep c s = if sep == c then empty else char c $> s
  in  removeIfSep ' ' Space <|> removeIfSep '\t' Tab

spaces :: CharParsing m => Separator -> m Spaces
spaces = fmap V.fromList . many . space

-- | Combinator to parse some data surrounded by spaces
spaced :: CharParsing m => Separator -> m a -> m (Spaced a)
spaced sep p = betwixt <$> spaces sep <*> p <*> spaces sep

-- | Parse an entire record, or "row"
record :: CharParsing m => ParseOptions s -> m (Record s)
record opts =
  let sep = view separator opts
      str = view encodeString opts
  in  Record . V.fromNel <$> (spacedField sep str `sepEndByNonEmpty` char sep)

-- | Parse many records, or "rows"
records :: CharParsing m => ParseOptions s -> m (Records s)
records opts =
  let manyV = fmap V.fromList . many
  in  fromMaybe EmptyRecords <$>
    optional (Records <$> firstRecord opts <*> manyV (subsequentRecord opts))

firstRecord :: CharParsing m => ParseOptions s -> m (Record s)
firstRecord opts = notFollowedBy (try (ending opts)) *> record opts

subsequentRecord :: CharParsing m => ParseOptions s -> m (Newline, Record s)
subsequentRecord opts =
  (,)
    <$> (notFollowedBy (try (ending opts)) *> newline) -- ((if view endOnBlankLine opts then (void newline) else empty) <|> eof))
    <*> record opts

-- | Parse zero or many newlines
ending :: CharParsing m => ParseOptions s -> m [Newline]
ending opts =
  let end = if view endOnBlankLine opts then pure [] else many newline
  in  [] <$ eof
    <|> try (pure <$> newline <* eof)
    <|> (:) <$> newline <*> ((:) <$> newline <*> end)

-- | Maybe parse the header row of a CSV file, depending on the given 'Headedness'
header :: CharParsing m => ParseOptions s -> m (Maybe (Header s))
header opts = case view headedness opts of
  Unheaded -> pure noHeader
  Headed -> mkHeader <$> record opts <*> newline

-- | Parse an 'Sv'
separatedValues :: CharParsing m => ParseOptions s -> m (Sv s)
separatedValues opts =
  Sv (view separator opts) <$> header opts <*> records opts <*> ending opts
