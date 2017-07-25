module Data.CSV.Parser where

import           Control.Applicative     (Alternative, (<$>), (<|>))
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet
import           Data.Functor            (void, ($>))
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, many, sepEndBy, some, try)

singleQuote, doubleQuote, backslash, comma, pipe, tab :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'
comma = ','
pipe = '|'
tab = '\t'

concatMany, concatSome :: Alternative f => f [a] -> f [a]
concatMany p = concat <$> many p
concatSome p = concat <$> some p

singleQuotedField, doubleQuotedField :: CharParsing m => m String
singleQuotedField = quotedField singleQuote
doubleQuotedField = quotedField doubleQuote

quotedField :: CharParsing m => Char -> m String
quotedField quote =
  let cq = char quote
      quoted = between cq cq
  in spaced (quoted (many (escapeChar quote <|> notChar quote)))

escapeChar :: CharParsing m => Char -> m Char
escapeChar c = try (string (concatenate backslash c)) $> c

concatenate :: Char -> Char -> String
concatenate c d = [c,d]

unquotedField :: CharParsing m => Char -> m String
unquotedField sep =
  many (
    escapeChar sep <|>
      noneOfSet (newlineOr sep)
  )

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

field :: CharParsing m => Char -> m String
field sep = choice [try singleQuotedField, try doubleQuotedField, unquotedField sep]

horizontalSpace :: CharParsing m => m Char
horizontalSpace = choice (fmap char [' ', '\t'])

spaced :: CharParsing m => m a -> m a
spaced =
  let s = many horizontalSpace
  in between s s

record :: CharParsing m => Char -> m [String]
record sep = field sep `sepEndBy` char sep

beginning :: CharParsing m => m ()
beginning = void $ many (oneOfSet newlines)

separatedValues :: CharParsing m => Char -> m [[String]]
separatedValues sep = beginning *> values sep

values :: CharParsing m => Char -> m [[String]]
values sep = record sep `sepEndBy` some (oneOfSet newlines)

