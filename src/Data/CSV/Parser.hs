module Data.CSV.Parser where

import           Control.Applicative     ((<|>), liftA3)
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.Functor            (void, ($>), (<$>))
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, sepEndBy, sepEndBy1, some, try)

import           Data.CSV.Field          (Field (UnquotedF, QuotedF) )
import           Data.CSV.Record         (Record (Record) )
import           Text.Between            (Between (Between), betwixt)
import           Text.Quote              (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

singleQuote, doubleQuote, backslash, comma, pipe, tab :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'
comma = ','
pipe = '|'
tab = '\t'

singleQuotedField, doubleQuotedField :: CharParsing m => m (Field String String)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m String -> m (Quoted String)
quoted q p =
  let c = char (quoteChar q)
  in  (fmap (Quoted q)) (between c c p)

quotedField :: CharParsing m => Quote -> m (Field String String)
quotedField quote =
  let qc = quoteChar quote
      escape = escapeQuote quote
  in  QuotedF <$> spaced (quoted quote (many (escape <|> notChar qc)))

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
  in liftA3 betwixt s p s

record :: CharParsing m => Char -> m (Record String String)
record sep =
  Record <$> field sep `sepEndBy1` char sep

beginning :: CharParsing m => m ()
beginning = void $ many (oneOfSet newlines)

separatedValues :: CharParsing m => Char -> m [Record String String]
separatedValues sep = beginning *> values sep

values :: CharParsing m => Char -> m [Record String String]
values sep =
  eof $> []
    <|> record sep `sepEndBy` some (oneOfSet newlines)
