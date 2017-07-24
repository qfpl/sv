module Data.CSV.Parser where

import           Control.Applicative     ((<$>), (<|>))
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, spaces, string)
import           Text.Parser.Combinators (between, choice, many)

singleQuote, doubleQuote, backslash :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'

singleQuotedField, doubleQuotedField :: CharParsing m => m String
singleQuotedField = quotedField singleQuote
doubleQuotedField = quotedField doubleQuote

quotedField :: CharParsing m => Char -> m String
quotedField quote =
  let cq = char quote
  in spaced (between cq cq (escapeChar quote <|> many (notChar quote)))

escapeChar :: CharParsing m => Char -> m String
escapeChar c = string (concatenate backslash c)

concatenate :: Char -> Char -> String
concatenate c d = [c,d]

unquotedField :: CharParsing m => Char -> m String
unquotedField sep =
  concat <$> many (
    escapeChar sep <|>
      (pure <$> noneOfSet (newlineOr sep))
  )

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

field :: CharParsing m => Char -> m String
field sep = choice [singleQuotedField, doubleQuotedField, unquotedField sep]

spaced :: CharParsing m => m a -> m a
spaced = between spaces spaces

