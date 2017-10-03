{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.Csv.ParserTest (test_Parser) where

import           Control.Lens         ((^.), review)
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Either          (isLeft)
import           Data.Foldable        (fold)
import           Data.Text            (Text)
import qualified Data.Text as Text    (singleton)
import           Data.Text1           (Text1, packed1)
import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (Assertion, assertBool, testCase, (@?=))
import           Text.Newline         (Newline (CR, LF, CRLF), newlineText)
import           Text.Parser.Char     (CharParsing)
import           Text.Parsec          (ParseError, parse)

import           Data.Csv.Csv         (Csv, mkCsv')
import           Data.Csv.Field       (Field (QuotedF, UnquotedF), MonoField, downmix, upmix)
import           Data.Csv.Parser.Internal (comma, ending, field, pipe, doubleQuotedField, record, separatedValues, singleQuotedField)
import           Data.Csv.Record      (Record (Record), NonEmptyRecord (SingleFieldNER), final, noFinal, FinalRecord, singleFinal)
import           Data.Separated       (sprinkle)
import           Text.Between         (betwixt, uniform)
import           Text.Escaped         (escapeNel, noEscape)
import           Text.Space           (Spaces (Spaces))
import           Text.Quote           (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    singleQuotedFieldTest
  , doubleQuotedFieldTest
  , fieldTest
  , recordTest
  , finalRecordTest
  , csvTest
  , psvTest
  ]

(@?=/) ::
  (Show a, Show b, Eq a, Eq b)
  => Either a b
  -> b
  -> Assertion
(@?=/) l r = l @?= Right r

qd, qs :: a -> Quoted a
qd = Quoted DoubleQuote . noEscape
qs = Quoted SingleQuote . noEscape
uq :: s -> MonoField s
uq = downmix . UnquotedF
uqa :: NonEmpty s -> Record s
uqa = Record . fmap uq
uqaa :: [NonEmpty s] -> [Record s]
uqaa = fmap uqa
nospc :: Quoted s2 -> Field s1 s2
nospc = QuotedF . uniform mempty

quotedFieldTest :: (forall m . CharParsing m => m (Field Text Text)) -> TestName -> Quote -> TestTree
quotedFieldTest parser name quote =
  let p = parse parser "" . concat
      q = [review quoteChar quote]
      qq = Quoted quote . noEscape
  in testGroup name [
    testCase "empty" $
      p [q,q] @?=/ nospc (qq "")
  , testCase "text" $
      p [q,"hello text",q]
        @?=/ nospc (qq "hello text")
  , testCase "capture space" $
      p ["   ", q, " spaced text  ", q, "     "]
        @?=/ QuotedF (betwixt (Spaces 3) (Spaces 5) (qq " spaced text  "))
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
  let p :: Text -> Either ParseError (Field Text Text)
      p = parse (field comma) ""
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ nospc (qd "hello")
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ nospc (qs "goodbye")
  , testCase "unquoted" $
      p "yes" @?=/ upmix (uq "yes")
  , testCase "spaced doublequoted" $
     p "       \" spaces  \"    " @?=/ QuotedF (betwixt (Spaces 7) (Spaces 4) (qd " spaces  "))
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ QuotedF (betwixt (Spaces 8) (Spaces 1) (qs " more spaces "))
  , testCase "spaced unquoted" $
     p "  text  " @?=/ upmix (uq "  text  ")
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ nospc (qs "hello,there,")
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ nospc (qd "court,of,the,,,,crimson,king")
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ upmix (uq "close")
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ upmix (uq "")
  ]

recordTest :: TestTree
recordTest =
  let p :: Text -> Either ParseError (Record Text)
      p = parse (record comma) ""
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

sqf :: Text -> FinalRecord b Text
sqf = final . SingleFieldNER . QuotedF . pure . qs
uqf :: NonEmpty Char -> FinalRecord Text1 b
uqf = final . SingleFieldNER . UnquotedF . (^. packed1)

finalRecordTest :: TestTree
finalRecordTest =
  let p :: Text -> Either ParseError (FinalRecord Text1 Text)
      p = parse (ending comma) ""
  in  testGroup "finalRecord" [
    testCase "single character" $
      p "a" @?=/ uqf ('a':|[])
  , testCase "empty string" $
      p "''" @?=/ sqf ""
  ]

separatedValuesTest :: Char -> Newline -> TestTree
separatedValuesTest sep nl =
  let p :: Text -> Either ParseError (Csv Text1 Text)
      p = parse (separatedValues sep) ""
      ps = parse (separatedValues sep) "" . fold
      csv :: [Record s2] -> FinalRecord s1 s2 -> Csv s1 s2
      csv rs e = mkCsv' sep e $ sprinkle nl rs
      s = Text.singleton sep
      nls = newlineText nl
  in  testGroup "separatedValues" [
    testCase "empty" $
      p "" @?=/ csv [] noFinal
  , testCase "single empty quotes field" $ 
      p "''" @?=/ csv [] (sqf "")
  , testCase "single field, single record" $
      p "one" @?=/ csv [] (singleFinal 'o' "ne")
  , testCase "single field, multiple records" $
      ps ["one",nls,"un"] @?=/ csv [uqa (pure "one")] (singleFinal 'u' "n")
  , testCase "multiple fields, single record" $
      ps ["one", s, "two", nls] @?=/ csv (uqaa (pure ("one":|["two"]))) noFinal
  , testCase "multiple fields, multiple records" $
      ps ["one", s, "two", s, "three", nls, "un", s, "deux", s, "trois", nls]
        @?=/ csv (uqaa ["one":|["two", "three"] , "un":|["deux", "trois"]]) noFinal
  ]

csvTest, psvTest :: TestTree
csvTest =
  testGroup "csv" $ fmap (separatedValuesTest comma) [CR, LF, CRLF]

psvTest =
  testGroup "psv" $ fmap (separatedValuesTest pipe) [CR, LF, CRLF]

