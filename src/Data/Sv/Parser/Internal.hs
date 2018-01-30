{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains internal implementation details of sv's parser.
-- As the "Internal" name suggests, this file is exempt from the PVP. Depend
-- on this module at your own risk!
module Data.Sv.Parser.Internal (
  separatedValues
  , separatedValuesC
  , separatedValuesEof
  , csv
  , psv
  , tsv
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

import           Data.Sv.Config          (SvConfig, headedness, separator)
import           Data.Sv.Sv              (Sv (Sv), Header, mkHeader, noHeader, Headedness (Unheaded, Headed), Separator, comma, pipe, tab)
import           Data.Sv.Field           (Field (Unquoted, Quoted))
import           Data.Sv.Record          (Record (Record), Records (Records))
import           Text.Babel              (Textual)
import           Text.Escaped            (escapeNel)
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Space              (HorizontalSpace (Space, Tab), Spaced, betwixt)
import           Text.Quote              (Quote (SingleQuote, DoubleQuote), quoteChar)

-- These two functions are in newer versions of the parsers package, but in
-- order to maintain compatibility with older versions I've left them here.
sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

singleQuotedField, doubleQuotedField :: (CharParsing m, Textual s) => m (Field s)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

quotedField :: (CharParsing m, Textual s)=> Quote -> m (Field s)
quotedField quote =
  let q = review quoteChar quote
      c = char q
      escape = escapeQuote quote
      chunks = fmap fromString (many (notChar q)) `sepByNonEmpty` escape
  in  Quoted quote <$> between c c (escapeNel <$> chunks)

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

field :: (CharParsing m, Textual s) => Separator -> m (Field s)
field sep =
  choice [
    singleQuotedField
  , doubleQuotedField
  , unquotedField sep
  ]

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

spaced :: CharParsing m => Separator -> m a -> m (Spaced a)
spaced sep p = betwixt <$> spaces sep <*> p <*> spaces sep

record :: (CharParsing m, Textual s) => Separator -> m (Record s)
record sep =
  Record <$> (spacedField sep `sepEndByNonEmpty` char sep)

records :: (CharParsing m, Textual s) => Separator -> m (Records s)
records sep =
  Records <$> optional (
    Pesarated1 <$> (
      Separated1 <$> firstRecord sep <*> separated (subsequentRecord sep)
    )
  )

firstRecord :: (CharParsing m, Textual s) => Separator -> m (Record s)
firstRecord sep = notFollowedBy (try ending) *> record sep

subsequentRecord :: (CharParsing m, Textual s) => Separator -> m (Newline, Record s)
subsequentRecord sep = (,) <$> try (newline <* notFollowedBy (void newline <|> eof)) <*> record sep

separated :: CharParsing m => m (a,b) -> m (Separated a b)
separated ab = Separated <$> many ab

ending :: CharParsing m => m [Newline]
ending =
  [] <$ eof
  <|> try (pure <$> newline <* eof)
  <|> (:) <$> newline <*> ((:) <$> newline <*> many newline)

header :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Maybe (Header s))
header c h = case h of
  Unheaded -> pure noHeader
  Headed -> mkHeader <$> record c <*> newline

separatedValues :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Sv s)
separatedValues sep h =
  Sv sep <$> header sep h <*> records sep <*> ending

separatedValuesC :: (CharParsing m, Textual s) => SvConfig -> m (Sv s)
separatedValuesC c =
  let s = view separator c
      h = view headedness c
  in  separatedValues s h

separatedValuesEof :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Sv s)
separatedValuesEof sep h =
  separatedValues sep h <* eof

csv :: (CharParsing m, Textual s) => Headedness -> m (Sv s)
csv = separatedValues comma

psv :: (CharParsing m, Textual s) => Headedness -> m (Sv s)
psv = separatedValues pipe

tsv :: (CharParsing m, Textual s) => Headedness -> m (Sv s)
tsv = separatedValues tab
