{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Parser.Internal (
  separatedValues
  , header
  , field
  , field'
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
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, sepEndBy, try)

import           Data.Csv.Csv            (Csv (Csv), Header, mkHeader, noHeader, Headedness (Unheaded, Headed), Separator)
import           Data.Csv.Field          (Field, Field' (UnquotedF, QuotedF), downmix)
import           Data.Csv.Record         (NonEmptyRecord (SingleFieldNER), Record (Record), HasRecord (fields), Records, singletonRecords, FinalRecord (FinalRecord), multiFieldNER)
import           Data.List.NonEmpty.Extra (AsNonEmpty)
import           Text.Babel              (Textual, fromString, IsString1, fromString1)
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

singleQuotedField, doubleQuotedField :: (CharParsing m, Textual s) => m (Field' a s)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped' a) -> m (Quoted a)
quoted q p =
  let c = char (review quoteChar q)
  in  Quoted q <$> between c c p

quotedField :: (CharParsing m, Textual s)=> Quote -> m (Field' a s)
quotedField quote =
  let qc = review quoteChar quote
      escape = escapeQuote quote
      chunks = fmap fromString (many (notChar qc)) `sepByNonEmpty` escape
  in  QuotedF <$> spaced (quoted quote (escapeNel <$> chunks))

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = review quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

unquotedField :: CharParsing m => Separator -> (m Char -> m t) -> m (Field' t s)
unquotedField sep combinator = UnquotedF <$> combinator (fieldChar sep)

fieldChar :: CharParsing m => Separator -> m Char
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

generalisedField :: (CharParsing m, Textual s) => (m Char -> m t) -> Char -> m (Field' t s)
generalisedField combinator sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep combinator
  ]

field' :: (CharParsing m, Textual s) => Char -> m (Field' s s)
field1 :: (CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Char -> m (Field' t s)
field' = fmap (first fromString) . generalisedField many
field1 = fmap (first fromString1) . generalisedField some1

field :: (CharParsing m, Textual s) => Char -> m (Field s)
field = fmap downmix . field'

space :: CharParsing m => m HorizontalSpace
space = char ' ' $> Space <|> char '\t' $> Tab

spaces :: CharParsing m => m [HorizontalSpace]
spaces = many space

spaced :: CharParsing m => m a -> m (Spaced a)
spaced p = liftA3 Between spaces p spaces

record :: (CharParsing m, Textual s) => Separator -> m (Record s)
record sep =
  Record <$> (field sep `sepEndByNonEmpty` char sep)

separatedValues :: (Monad m, CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Separator -> Headedness -> m (Csv t s)
separatedValues sep h =
  fmap uncurry (Csv sep) <$> header sep h <*> multiRecords sep

records :: (CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Separator -> m (Either (FinalRecord t s) (Records s))
records sep =
  try (Left <$> ending sep)
  <|> (Right <$> (singletonRecords <$> record sep <*> newline))

multiRecords :: (Monad m, CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Separator -> m (Records s, FinalRecord t s)
multiRecords sep =
  untilLeft id (records sep)

untilLeft :: (Monoid r, Monad m) => (a -> r) -> m (Either l a) -> m (r, l)
untilLeft f x =
  x >>= \e -> case e of
    Left l  -> pure (mempty, l)
    Right r -> first (f r <>) <$> untilLeft f x

ending :: (CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Separator -> m (FinalRecord t s)
ending sep = (FinalRecord <$> optional (nonEmptyRecord sep)) <* eof

nonEmptyRecord :: (CharParsing m, Textual s, IsString1 t, AsNonEmpty t s) => Separator -> m (NonEmptyRecord t s)
nonEmptyRecord sep =
  try (multiFieldNER <$> field sep <* char sep <*> (view fields <$> record sep))
  <|> SingleFieldNER <$> field1 sep

header :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Maybe (Header s))
header c h = case h of
  Unheaded -> pure noHeader
  Headed -> mkHeader <$> record c <*> newline
