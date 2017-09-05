{-# LANGUAGE ScopedTypeVariables #-}

module Data.CSV.Parser where

import           Control.Applicative     (Alternative, (<|>), liftA3, optional)
import           Control.Lens            (view)
import           Data.Bifunctor          (Bifunctor (first))
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.Functor            (($>), (<$>))
import           Data.List.NonEmpty      (NonEmpty ((:|)), some1)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           Data.Text1              (Text1, packed1)
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, sepEndBy, try)

import           Data.CSV.CSV            (CSV (CSV), FinalRecord (FinalRecord), Records, singletonRecords)
import           Data.CSV.Field          (Field (UnquotedF, QuotedF), MonoField (MonoField))
import           Data.CSV.Record         (NonEmptyRecord (MultiFieldNER, SingleFieldNER), Record (Record, fields))
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

singleQuotedField, doubleQuotedField :: CharParsing m => m (Field Text a Text)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped a) -> m (Quoted a)
quoted q p =
  let c = char (quoteChar q)
  in  Quoted q <$> between c c p

quotedField :: CharParsing m => Quote -> m (Field Text a Text)
quotedField quote =
  let qc = quoteChar quote
      escape = escapeQuote quote
      noescape = fmap pack (many (notChar qc)) `sepByNonEmpty` escape
  in  QuotedF <$> spaced (quoted quote (SeparatedByEscapes <$> noescape))

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

unquotedField :: CharParsing m => Char -> (m Char -> m (f Char)) -> m (Field a (f Char) b)
unquotedField sep combinator = UnquotedF <$> combinator (fieldChar sep)

fieldChar :: CharParsing m => Char -> m Char
fieldChar sep = noneOfSet (newlineOr sep)

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

newline :: CharParsing m => m Newline
newline =
  CRLF <$ try (string "\r\n")
    <|> CR <$ char '\r'
    <|> LF <$ char '\n'

generalisedField :: CharParsing m => (m Char -> m (f Char)) -> Char -> m (Field Text (f Char) Text)
generalisedField combinator sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep combinator
  ]

field :: CharParsing m => Char -> m (Field Text Text Text)
field1 :: CharParsing m => Char -> m (Field Text Text1 Text)
field = fmap (first pack) . generalisedField many
field1 = fmap (first (view packed1)) . generalisedField some1

monoField :: CharParsing m => Char -> m (MonoField Text Text)
monoField = fmap MonoField . field

horizontalSpace :: CharParsing m => m Char
horizontalSpace = choice (fmap char [' ', '\t'])

spaced :: CharParsing m => m a -> m (Between Text a)
spaced p =
  let s = pack <$> many horizontalSpace
  in liftA3 Between s p s

record :: CharParsing m => Char -> m (Record Text Text)
record sep =
  Record <$> ((MonoField <$> field sep) `sepEndByNonEmpty` char sep)

separatedValues :: (Monad m, CharParsing m) => Char -> m (CSV Text Text1 Text)
separatedValues sep =
  uncurry (CSV sep) <$> multiRecords sep

records :: CharParsing m => Char -> m (Either (FinalRecord Text Text1 Text) (Records Text Text))
records sep =
  try (Left <$> ending sep)
  <|> (Right <$> (singletonRecords <$> record sep <*> newline))

multiRecords :: (Monad m, CharParsing m) => Char -> m (Records Text Text, FinalRecord Text Text1 Text)
multiRecords sep =
  untilLeft id (records sep)

untilLeft :: (Monoid r, Monad m) => (a -> r) -> m (Either l a) -> m (r, l)
untilLeft f x =
  x >>= \e -> case e of
    Left l  -> pure (mempty, l)
    Right r -> first (f r <>) <$> untilLeft f x

ending :: CharParsing m => Char -> m (FinalRecord Text Text1 Text)
ending sep = (FinalRecord <$> (optional (nonEmptyRecord sep))) <* eof

nonEmptyRecord :: CharParsing m => Char -> m (NonEmptyRecord Text Text1 Text)
nonEmptyRecord sep =
  try (MultiFieldNER <$> monoField sep <* char sep <*> (fields <$> record sep))
  <|> SingleFieldNER <$> field1 sep

