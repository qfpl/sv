{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
  , separatedValuesEof
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

import           Control.Applicative     (Alternative ((<|>), empty), optional)
import           Control.Lens            (review, view)
import           Data.CharSet            (CharSet, (\\))
import qualified Data.CharSet as CharSet (fromList, insert, singleton)
import           Data.Functor            (($>), (<$>), void)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.Semigroup          ((<>))
import           Data.Separated          (Pesarated1 (Pesarated1), Separated (Separated), Separated1 (Separated1))
import           Data.String             (IsString (fromString))
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, notFollowedBy, sepEndBy, try)

import           Data.Sv.Field           (Field (Unquoted, Quoted))
import           Data.Sv.Sv              (Sv (Sv), Header, mkHeader, noHeader, Headedness (Unheaded, Headed), Separator)
import           Data.Sv.Parse.Options   (ParseOptions, headedness, separator, endOnBlankLine)
import           Data.Sv.Record          (Record (Record), Records (Records))
import           Text.Babel              (Textual)
import           Text.Escape             (Unescaped (Unescaped))
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Space              (HorizontalSpace (Space, Tab), Spaced, betwixt)
import           Text.Quote              (Quote (SingleQuote, DoubleQuote), quoteChar)

-- | This function is in newer versions of the parsers package, but in
-- order to maintain compatibility with older versions I've left it here.
sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

-- | Parse a field surrounded by single quotes
singleQuotedField :: (CharParsing m, Textual s) => m (Field s)
singleQuotedField = quotedField SingleQuote

-- | Parse a field surrounded by double quotes
doubleQuotedField :: (CharParsing m, Textual s) => m (Field s)
doubleQuotedField = quotedField DoubleQuote

-- | Given a quote, parse its escaped form (which is it repeated twice)
escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

quotedField :: (CharParsing m, Textual s) => Quote -> m (Field s)
quotedField quote =
  let q = review quoteChar quote
      c = char q
      cc = escapeQuote quote
  in  Quoted quote . Unescaped . fromString <$> between c c (many (cc <|> notChar q))

-- | Parse a field that is not surrounded by quotes
unquotedField :: (IsString s, CharParsing m) => Separator -> m (Field s)
unquotedField sep =
  let spaceSet = CharSet.fromList " \t" \\ CharSet.singleton sep
      oneSpace = oneOfSet spaceSet
      nonSpaceFieldChar = noneOfSet (newlineOr sep <> spaceSet)
      terminalWhitespace = many oneSpace *> fieldEnder
      fieldEnder = void (oneOfSet (newlineOr sep)) <|> eof
  in  Unquoted . fromString <$>
    many (
      nonSpaceFieldChar
      <|> (notFollowedBy (try terminalWhitespace)) *> oneSpace
    )

-- | Parse a field, be it quoted or unquoted
field :: (CharParsing m, Textual s) => Separator -> m (Field s)
field sep =
  choice [
    singleQuotedField
  , doubleQuotedField
  , unquotedField sep
  ]

-- | Parse a field with its surrounding spacing
spacedField :: (CharParsing m, Textual s) => Separator -> m (Spaced (Field s))
spacedField = spaced <*> field

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

spaces :: CharParsing m => Separator -> m [HorizontalSpace]
spaces = many . space

-- | Combinator to parse some data surrounded by spaces
spaced :: CharParsing m => Separator -> m a -> m (Spaced a)
spaced sep p = betwixt <$> spaces sep <*> p <*> spaces sep

-- | Parse an entire record, or "row"
record :: (CharParsing m, Textual s) => Separator -> m (Record s)
record sep =
  Record <$> (spacedField sep `sepEndByNonEmpty` char sep)

-- | Parse many records, or "rows"
records :: (CharParsing m, Textual s) => ParseOptions -> m (Records s)
records opts =
  Records <$> optional (
    Pesarated1 <$> (
      Separated1 <$> firstRecord opts <*> separated (subsequentRecord opts)
    )
  )

firstRecord :: (CharParsing m, Textual s) => ParseOptions -> m (Record s)
firstRecord opts = notFollowedBy (try (ending opts)) *> record (view separator opts)

subsequentRecord :: (CharParsing m, Textual s) => ParseOptions -> m (Newline, Record s)
subsequentRecord opts =
  (,)
    <$> (notFollowedBy (try (ending opts)) *> newline) -- ((if view endOnBlankLine opts then (void newline) else empty) <|> eof))
    <*> record (view separator opts)

separated :: CharParsing m => m (a,b) -> m (Separated a b)
separated ab = Separated <$> many ab

-- | Parse zero or many newlines
ending :: CharParsing m => ParseOptions -> m [Newline]
ending opts =
  let end = if view endOnBlankLine opts then pure [] else many newline
  in  [] <$ eof
    <|> try (pure <$> newline <* eof)
    <|> (:) <$> newline <*> ((:) <$> newline <*> end)

-- | Maybe parse the header row of a CSV file, depending on the given 'Headedness'
header :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Maybe (Header s))
header c h = case h of
  Unheaded -> pure noHeader
  Headed -> mkHeader <$> record c <*> newline

-- | Parse an Sv
separatedValues :: (CharParsing m, Textual s) => ParseOptions -> m (Sv s)
separatedValues opts =
  let sep = view separator opts
      h   = view headedness opts
  in  Sv sep <$> header sep h <*> records opts <*> ending opts

-- | Parse an Sv and ensure the end of the file follows.
separatedValuesEof :: (CharParsing m, Textual s) => ParseOptions -> m (Sv s)
separatedValuesEof opts =
  separatedValues opts <* eof
