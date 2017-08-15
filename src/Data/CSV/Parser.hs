{-# LANGUAGE ScopedTypeVariables #-}

module Data.CSV.Parser where

import           Control.Applicative     (Alternative, (<|>), liftA3)
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.Functor            (void, ($>), (<$>))
import           Data.Separated          (pesaratedBy1)
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, sepEndBy, try)

import           Data.CSV.CSV            (CSV (CSV), Records (Records))
import           Data.CSV.Field          (Field (UnquotedF, QuotedF) )
import           Data.CSV.Record         (Record (Record) )
import           Text.Between            (Between (Between))
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Quote              (Escaped (SeparatedByEscapes), Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

singleQuote, doubleQuote, backslash, comma, pipe, tab :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'
comma = ','
pipe = '|'
tab = '\t'

sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

singleQuotedField, doubleQuotedField :: CharParsing m => m (Field String String)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped a) -> m (Quoted a)
quoted q p =
  let c = char (quoteChar q)
  in  (fmap (Quoted q)) (between c c p)

quotedField :: CharParsing m => Quote -> m (Field String String)
quotedField quote =
  let qc = quoteChar quote
      escape = escapeQuote quote
  in  QuotedF <$> spaced (quoted quote (SeparatedByEscapes <$> (many (notChar qc) `sepByNonEmpty` escape)))

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

unquotedField :: CharParsing m => Char -> m (Field String String)
unquotedField sep =
  UnquotedF <$>
    many (
      noneOfSet (newlineOr sep)
    )

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

newline :: CharParsing m => m Newline
newline =
  CRLF <$ try (string "\r\n")
    <|> CR <$ char '\r'
    <|> LF <$ char '\n'

field :: CharParsing m => Char -> m (Field String String)
field sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep
  ]

horizontalSpace :: CharParsing m => m Char
horizontalSpace = choice (fmap char [' ', '\t'])

spaced :: CharParsing m => m a -> m (Between String a)
spaced p =
  let s = many horizontalSpace
  in liftA3 Between s p s

record :: CharParsing m => Char -> m (Record String String)
record sep =
  Record <$> field sep `sepEndByNonEmpty` char sep

beginning :: CharParsing m => m ()
beginning = void $ many (oneOfSet newlines)

separatedValues :: CharParsing m => Char -> m (CSV String String)
separatedValues sep =
  beginning *> (CSV sep <$> values sep <*> ending)

values :: CharParsing m => Char -> m (Records String String)
values sep =
  (Records Nothing <$ eof)
    <|> (Records . Just) <$> (record sep `pesaratedBy1` newline)

ending :: CharParsing m => m [Newline]
ending = many newline

