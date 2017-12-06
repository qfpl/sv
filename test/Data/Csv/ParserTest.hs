{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.Csv.ParserTest (test_Parser) where

import           Data.ByteString      (ByteString)
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Either          (isLeft)
import           Data.Foldable        (fold)
import           Data.Text            (Text)
import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (Assertion, assertBool, testCase, (@?=))
import           Text.Newline         (Newline (CR, LF, CRLF), newlineText)
import           Text.Parser.Char     (CharParsing)
import           Text.Trifecta        (Result (Success, Failure), parseByteString, _errDoc)

import           Data.Csv.Csv         (Csv, mkCsv', comma, pipe, tab, Headedness (Unheaded), Separator)
import           Data.Csv.Field       (Field (QuotedF, UnquotedF))
import           Data.Csv.Parser.Internal (field, doubleQuotedField, record, separatedValues, singleQuotedField)
import           Data.Csv.Record      (Record (Record))
import           Data.Separated       (skrinpleMay)
import           Text.Babel           (singleton)
import           Text.Between         (uniform)
import           Text.Escaped         (escapeNel, noEscape)
import           Text.Space           (manySpaces, spaced)
import           Text.Quote           (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteToString)

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    singleQuotedFieldTest
  , doubleQuotedFieldTest
  , fieldTest
  , recordTest
  , csvTest
  , psvTest
  , tsvTest
  , nsvTest
  , crsvTest
  , bssvTest
  ]

r2e :: Result a -> Either String a
r2e r = case r of
  Success a -> Right a
  Failure e -> Left (show (_errDoc e))

(@?=/) ::
  (Show a, Show b, Eq a, Eq b)
  => Either a b
  -> b
  -> Assertion
(@?=/) l r = l @?= pure r

qd, qs :: a -> Quoted a
qd = Quoted DoubleQuote . noEscape
qs = Quoted SingleQuote . noEscape
qsr :: s -> Record s
qsr = Record . pure . nospc . qs
uq :: s -> Field s
uq = UnquotedF
uqa :: NonEmpty s -> Record s
uqa = Record . fmap uq
uqaa :: [NonEmpty s] -> [Record s]
uqaa = fmap uqa
nospc :: Quoted s -> Field s
nospc = QuotedF . uniform mempty

quotedFieldTest :: (forall m . CharParsing m => m (Field Text)) -> TestName -> Quote -> TestTree
quotedFieldTest parser name quote =
  let p :: [ByteString] -> Either String (Field Text)
      p = r2e . parseByteString parser mempty . mconcat
      q = quoteToString quote
      qq = Quoted quote . noEscape
  in testGroup name [
    testCase "empty" $
      p [q,q] @?=/ nospc (qq "")
  , testCase "text" $
      p [q,"hello text",q]
        @?=/ nospc (qq "hello text")
  , testCase "capture space" $
      p ["   ", q, " spaced text  ", q, "     "]
        @?=/ QuotedF (spaced (manySpaces 3) (manySpaces 5) (qq " spaced text  "))
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  , testCase "quoted field can handle escaped quotes" $
     p [q,"yes", q, q, "no", q] @?=/ nospc (Quoted quote (escapeNel ("yes" :| ["no"])))
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" SingleQuote
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" DoubleQuote

fieldTest :: TestTree
fieldTest =
  let p :: ByteString -> Either String (Field Text)
      p = r2e . parseByteString (field comma) mempty
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ nospc (qd "hello")
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ nospc (qs "goodbye")
  , testCase "unquoted" $
      p "yes" @?=/ uq "yes"
  , testCase "spaced doublequoted" $
     p "       \" spaces  \"    " @?=/ QuotedF (spaced (manySpaces 7) (manySpaces 4) (qd " spaces  "))
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ QuotedF (spaced (manySpaces 8) (manySpaces 1) (qs " more spaces "))
  , testCase "spaced unquoted" $
     p "  text  " @?=/ uq "  text  "
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ nospc (qs "hello,there,")
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ nospc (qd "court,of,the,,,,crimson,king")
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ uq "close"
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ uq ""
  ]

recordTest :: TestTree
recordTest =
  let p :: ByteString -> Either String (Record Text)
      p = r2e . parseByteString (record comma) mempty
  in  testGroup "record" [
    testCase "single field" $
      p "Yes" @?=/ uqa (pure "Yes")
  , testCase "fields" $
      p "Anderson,Squire,Wakeman,Howe,Bruford" @?=/ uqa ("Anderson":|["Squire", "Wakeman", "Howe", "Bruford"])
  , testCase "commas" $
      p ",,," @?=/ uqa ("":|["","",""])
  , testCase "record ends at newline" $
      p "a,b,c\nd,e,f" @?=/ uqa ("a":|["b","c"])
  , testCase "record ends at carriage return" $
      p "g,h,i\rj,k,l" @?=/ uqa ("g":|["h","i"])
  , testCase "record ends at carriage return followed by newline" $
      p "m,n,o\r\np,q,r" @?=/ uqa ("m":|["n","o"])
  ]

separatedValuesTest :: Separator -> Newline -> Bool -> TestTree
separatedValuesTest sep nl terminatedByNewline =
  let p :: ByteString -> Either String (Csv Text)
      p = r2e . parseByteString (separatedValues sep Unheaded) mempty
      ps = p . fold
      csv :: [Record s] -> [Newline] -> Csv s
      csv rs e = mkCsv' sep Nothing e $ skrinpleMay nl rs
      s = singleton sep
      nls = newlineText nl
      terminator = if terminatedByNewline then [nl] else []
      termStr = foldMap newlineText terminator
  in  testGroup "separatedValues" [
    testCase "empty" $
      ps ["", termStr] @?=/ csv [] terminator
  , testCase "single empty quotes field" $ 
      ps ["''", termStr] @?=/ csv [qsr ""] terminator
  , testCase "single field, single record" $
      ps ["one", termStr] @?=/ csv [uqa (pure "one")] terminator
  , testCase "single field, multiple records" $
      ps ["one",nls,"un",termStr] @?=/ csv [uqa (pure "one"), uqa (pure "un")] terminator
  , testCase "multiple fields, single record" $
      ps ["one", s, "two",termStr] @?=/ csv (uqaa (pure ("one":|["two"]))) terminator
  , testCase "multiple fields, multiple records" $
      ps ["one", s, "two", s, "three", nls, "un", s, "deux", s, "trois",termStr]
        @?=/ csv (uqaa ["one":|["two", "three"] , "un":|["deux", "trois"]]) terminator
  ]

svTest :: String -> Separator -> TestTree
svTest name sep =
  testGroup name $ separatedValuesTest sep <$> [CR, LF, CRLF] <*> [False, True]

csvTest :: TestTree
csvTest = svTest "csv" comma

psvTest :: TestTree
psvTest = svTest "psv" pipe

tsvTest :: TestTree
tsvTest = svTest "tsv" tab

nsvTest :: TestTree
nsvTest = svTest "NULL separated values" '\0'

crsvTest :: TestTree
crsvTest =
  testGroup "carriage return separated values" $
    separatedValuesTest '\r' <$> [LF] <*> [False, True]

bssvTest :: TestTree
bssvTest = svTest "backspace separated values" '\BS'
