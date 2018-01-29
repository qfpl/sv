{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.ParserTest (test_Parser) where

import           Data.ByteString      (ByteString)
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Either          (isLeft)
import           Data.Foldable        (fold)
import           Data.Text            (Text)
import           Hedgehog
import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog  (testProperty)
import           Test.Tasty.HUnit     (Assertion, assertBool, testCase, (@?=))
import           Text.Newline         (Newline (CR, LF, CRLF), newlineText)
import           Text.Parser.Char     (CharParsing)
import           Text.Trifecta        (Result (Success, Failure), parseByteString, _errDoc)

import           Data.Sv.Sv          (Sv, mkSv', comma, pipe, tab, Headedness (Unheaded), Separator)
import           Data.Sv.Field       (Field (Quoted, Unquoted), SpacedField)
import           Data.Sv.Generators  (genCsvString)
import           Data.Sv.Parser.Internal (csv, doubleQuotedField, record, separatedValues, singleQuotedField, spaced, spacedField)
import           Data.Sv.Record      (Record (Record))
import           Data.Separated      (skrinpleMay)
import           Text.Babel          (singleton)
import           Text.Escaped        (escapeNel, noEscape)
import           Text.Space          (Spaced (Spaced), manySpaces, noSpaces)
import           Text.Quote          (Quote (SingleQuote, DoubleQuote), quoteToString)

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
  , randomCsvTest
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

qd, qs :: a -> Field a
qd = Quoted DoubleQuote . noEscape
qs = Quoted SingleQuote . noEscape
qsr :: s -> Record s
qsr = Record . pure . nospc . qs
uq :: s -> SpacedField s
uq = noSpaces . Unquoted
uqa :: NonEmpty s -> Record s
uqa = Record . fmap uq
uqaa :: [NonEmpty s] -> [Record s]
uqaa = fmap uqa
nospc :: Field s -> SpacedField s
nospc = noSpaces

quotedFieldTest :: (forall m . CharParsing m => m (SpacedField Text)) -> TestName -> Quote -> TestTree
quotedFieldTest parser name quote =
  let p :: [ByteString] -> Either String (SpacedField Text)
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
        @?=/ Spaced (manySpaces 3) (manySpaces 5) (qq " spaced text  ")
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
singleQuotedFieldTest = quotedFieldTest (spaced comma singleQuotedField) "singleQuotedField" SingleQuote
doubleQuotedFieldTest = quotedFieldTest (spaced comma doubleQuotedField) "doubleQuotedField" DoubleQuote

fieldTest :: TestTree
fieldTest =
  let p :: ByteString -> Either String (SpacedField Text)
      p = r2e . parseByteString (spacedField comma) mempty
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ nospc (qd "hello")
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ nospc (qs "goodbye")
  , testCase "unquoted" $
      p "yes" @?=/ uq "yes"
  , testCase "spaced doublequoted" $
      p "       \" spaces  \"    " @?=/ Spaced (manySpaces 7) (manySpaces 4) (qd " spaces  ")
  , testCase "spaced singlequoted" $
      p "        ' more spaces ' " @?=/ Spaced (manySpaces 8) (manySpaces 1) (qs " more spaces ")
  , testCase "spaced unquoted" $
      p "  some text   " @?=/ Spaced (manySpaces 2) (manySpaces 3) (Unquoted "some text")
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

separatedValuesTest :: Separator -> Newline -> Int -> TestTree
separatedValuesTest sep nl newlines =
  let p :: ByteString -> Either String (Sv Text)
      p = r2e . parseByteString (separatedValues sep Unheaded) mempty
      ps = p . fold
      mkSv'' :: [Record s] -> [Newline] -> Sv s
      mkSv'' rs e = mkSv' sep Nothing e $ skrinpleMay nl rs
      s = singleton sep
      nls = newlineText nl
      terminator = replicate newlines nl
      termStr = foldMap newlineText terminator
  in  testGroup "separatedValues" [
    testCase "empty" $
      ps ["", termStr] @?=/ mkSv'' [] terminator
  , testCase "single empty quotes field" $ 
      ps ["''", termStr] @?=/ mkSv'' [qsr ""] terminator
  , testCase "single field, single record" $
      ps ["one", termStr] @?=/ mkSv'' [uqa (pure "one")] terminator
  , testCase "single field, multiple records" $
      ps ["one",nls,"un",termStr] @?=/ mkSv'' [uqa (pure "one"), uqa (pure "un")] terminator
  , testCase "multiple fields, single record" $
      ps ["one", s, "two",termStr] @?=/ mkSv'' (uqaa (pure ("one":|["two"]))) terminator
  , testCase "multiple fields, multiple records" $
      ps ["one", s, "two", s, "three", nls, "un", s, "deux", s, "trois",termStr]
        @?=/ mkSv'' (uqaa ["one":|["two", "three"] , "un":|["deux", "trois"]]) terminator
  ]

svTest :: String -> Separator -> TestTree
svTest name sep =
  testGroup name $ separatedValuesTest sep <$> [CR, LF, CRLF] <*> [0,1,2]

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
    separatedValuesTest '\r' <$> [LF] <*> [0,1,2]

bssvTest :: TestTree
bssvTest = svTest "backspace separated values" '\BS'

prop_randomCsvTest :: Property
prop_randomCsvTest = property $ do
  str <- forAll genCsvString
  let x :: Either String (Sv String)
      x = r2e (parseByteString (csv Unheaded) mempty str)
  case x of
    Left _ -> failure
    Right _ -> success

randomCsvTest :: TestTree
randomCsvTest =
  testProperty "parse random CSV" prop_randomCsvTest
