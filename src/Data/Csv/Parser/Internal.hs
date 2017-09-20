{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Parser.Internal (
  separatedValues
  , comma
  , pipe
  , field
  , monoField
  , singleQuotedField
  , doubleQuotedField
  , unquotedField
  , record
  , ending
) where

import           Control.Applicative     (Alternative, (<|>), liftA3, optional)
import           Control.Lens            (review, view)
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

import           Data.Csv.Csv            (Csv (Csv))
import           Data.Csv.Field          (Field (UnquotedF, QuotedF), MonoField, downmix)
import           Data.Csv.Record         (NonEmptyRecord (SingleFieldNER), Record (Record), HasRecord (fields), Records, singletonRecords, FinalRecord (FinalRecord), multiFieldNER)
import           Text.Between            (Between (Between))
import           Text.Escaped            (Escaped', escapeNel)
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Space              (Spaced, single)
import           Text.Quote              (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

-- | The comma character
comma :: Char
comma = ','

-- | The pipe character
pipe :: Char
pipe = '|'

-- These two functions are in newer versions of the parsers package, but in
-- order to maintain compatibility with older versions I've left them here.
sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

singleQuotedField, doubleQuotedField :: CharParsing m => m (Field a Text)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped' a) -> m (Quoted a)
quoted q p =
  let c = char (review quoteChar q)
  in  Quoted q <$> between c c p

quotedField :: CharParsing m => Quote -> m (Field a Text)
quotedField quote =
  let qc = review quoteChar quote
      escape = escapeQuote quote
      chunks = fmap pack (many (notChar qc)) `sepByNonEmpty` escape
  in  QuotedF <$> spaced (quoted quote (escapeNel <$> chunks))

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

unquotedField :: CharParsing m => Char -> (m Char -> m (f Char)) -> m (Field (f Char) b)
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

generalisedField :: CharParsing m => (m Char -> m (f Char)) -> Char -> m (Field (f Char) Text)
generalisedField combinator sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep combinator
  ]

field :: CharParsing m => Char -> m (Field Text Text)
field1 :: CharParsing m => Char -> m (Field Text1 Text)
field = fmap (first pack) . generalisedField many
field1 = fmap (first (view packed1)) . generalisedField some1

monoField :: CharParsing m => Char -> m (MonoField Text)
monoField = fmap downmix . field

spaced :: CharParsing m => m a -> m (Spaced a)
spaced p =
  let s = foldMap (const single) <$> many (char ' ')
  in liftA3 Between s p s

record :: CharParsing m => Char -> m (Record Text)
record sep =
  Record <$> ((downmix <$> field sep) `sepEndByNonEmpty` char sep)

separatedValues :: (Monad m, CharParsing m) => Char -> m (Csv Text1 Text)
separatedValues sep =
  uncurry (Csv sep) <$> multiRecords sep

records :: CharParsing m => Char -> m (Either (FinalRecord Text1 Text) (Records Text))
records sep =
  try (Left <$> ending sep)
  <|> (Right <$> (singletonRecords <$> record sep <*> newline))

multiRecords :: (Monad m, CharParsing m) => Char -> m (Records Text, FinalRecord Text1 Text)
multiRecords sep =
  untilLeft id (records sep)

untilLeft :: (Monoid r, Monad m) => (a -> r) -> m (Either l a) -> m (r, l)
untilLeft f x =
  x >>= \e -> case e of
    Left l  -> pure (mempty, l)
    Right r -> first (f r <>) <$> untilLeft f x

ending :: CharParsing m => Char -> m (FinalRecord Text1 Text)
ending sep = (FinalRecord <$> optional (nonEmptyRecord sep)) <* eof

nonEmptyRecord :: CharParsing m => Char -> m (NonEmptyRecord Text1 Text)
nonEmptyRecord sep =
  try (multiFieldNER <$> monoField sep <* char sep <*> (view fields <$> record sep))
  <|> SingleFieldNER <$> field1 sep

