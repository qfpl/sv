module Data.CSV.Parser where

import           Control.Applicative     ((<|>))
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.Functor            (void, ($>))
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, sepEndBy, sepEndBy1, some, try)

import Data.CSV.Field (Field ( Unquoted, Quoted) )

singleQuote, doubleQuote, backslash, comma, pipe, tab :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'
comma = ','
pipe = '|'
tab = '\t'

singleQuotedField, doubleQuotedField :: CharParsing m => m Field
singleQuotedField = quotedField singleQuote
doubleQuotedField = quotedField doubleQuote

between' :: CharParsing m => Char -> m String -> m Field
between' c p =
  let cc = char c
  in  fmap (Quoted c) (between cc cc p)

quotedField :: CharParsing m => Char -> m Field
quotedField quote =
  let quoted = between' quote
  in  spaced (quoted (many (escapeChar quote <|> notChar quote)))

escapeChar :: CharParsing m => Char -> m Char
escapeChar c = try (string (concatenate backslash c)) $> c

concatenate :: Char -> Char -> String
concatenate c d = [c,d]

unquotedField :: CharParsing m => Char -> m Field
unquotedField sep =
  Unquoted <$>
    many (
      escapeChar sep <|>
        noneOfSet (newlineOr sep)
    )

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

field :: CharParsing m => Char -> m Field
field sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep
  ]

horizontalSpace :: CharParsing m => m Char
horizontalSpace = choice (fmap char [' ', '\t'])

spaced :: CharParsing m => m a -> m a
spaced =
  let s = many horizontalSpace
  in between s s

record :: CharParsing m => Char -> m [Field]
record sep =
  field sep `sepEndBy1` char sep

beginning :: CharParsing m => m ()
beginning = void $ many (oneOfSet newlines)

separatedValues :: CharParsing m => Char -> m [[Field]]
separatedValues sep = beginning *> values sep

values :: CharParsing m => Char -> m [[Field]]
values sep =
  eof $> []
    <|> record sep `sepEndBy` some (oneOfSet newlines)
