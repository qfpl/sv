{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Parser.Internal (
  separatedValues
  , header
  , field
  , singleQuotedField
  , doubleQuotedField
  , unquotedField
  , record
  , ending
) where

import           Control.Applicative     (Alternative, (<|>), optional)
import           Control.Lens            (review)
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.Functor            (($>), (<$>))
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Data.Separated          (Pesarated1 (Pesarated1), Separated (Separated), Separated1 (Separated1))
import           Data.String             (IsString (fromString))
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, notFollowedBy, sepEndBy, try)

import           Data.Csv.Csv            (Csv (Csv), Header, mkHeader, noHeader, Headedness (Unheaded, Headed), Separator)
import           Data.Csv.Field          (Field (UnquotedF, QuotedF))
import           Data.Csv.Record         (Record (Record), Records (Records))
import           Text.Babel              (Textual)
import           Text.Between            (Between (Between))
import           Text.Escaped            (Escaped', escapeNel)
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Space              (HorizontalSpace (Space, Tab), Spaced)
import           Text.Quote              (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

-- These two functions are in newer versions of the parsers package, but in
-- order to maintain compatibility with older versions I've left them here.
sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

singleQuotedField, doubleQuotedField :: (CharParsing m, Textual s) => m (Field s)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped' a) -> m (Quoted a)
quoted q p =
  let c = char (review quoteChar q)
  in  Quoted q <$> between c c p

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

quotedField :: (CharParsing m, Textual s)=> Quote -> m (Field s)
quotedField quote =
  let qc = review quoteChar quote
      escape = escapeQuote quote
      chunks = fmap fromString (many (notChar qc)) `sepByNonEmpty` escape
  in  QuotedF <$> spaced (quoted quote (escapeNel <$> chunks))

unquotedField :: IsString s => CharParsing m => Separator -> m (Field s)
unquotedField sep = UnquotedF . fromString <$> many (fieldChar sep)

fieldChar :: CharParsing m => Separator -> m Char
fieldChar sep = noneOfSet (newlineOr sep)

field :: (CharParsing m, Textual s) => Separator -> m (Field s)
field sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep
  ]

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

newline :: CharParsing m => m Newline
newline =
  CRLF <$ try (string "\r\n")
    <|> CR <$ char '\r'
    <|> LF <$ char '\n'

space :: CharParsing m => m HorizontalSpace
space = char ' ' $> Space <|> char '\t' $> Tab

spaces :: CharParsing m => m [HorizontalSpace]
spaces = many space

spaced :: CharParsing m => m a -> m (Spaced a)
spaced p = Between <$> spaces <*> p <*> spaces

record :: (CharParsing m, Textual s) => Separator -> m (Record s)
record sep =
  Record <$> (field sep `sepEndByNonEmpty` char sep)

separatedValues :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Csv s)
separatedValues sep h =
  Csv sep <$> header sep h <*> records sep <*> ending

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
subsequentRecord sep = (,) <$> try (newline <* notFollowedBy eof) <*> record sep

separated :: CharParsing m => m (a,b) -> m (Separated a b)
separated ab = Separated <$> many ab

ending :: CharParsing m => m [Newline]
ending = many newline <* eof

header :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Maybe (Header s))
header c h = case h of
  Unheaded -> pure noHeader
  Headed -> mkHeader <$> record c <*> newline
