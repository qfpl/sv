{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.CSV.ParserTest (test_Parser) where

import           Control.Lens         ((^.))
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Either          (isLeft)
import           Data.Text            (Text)
import           Data.Text1           (Text1, packed1)
import           Test.Tasty           (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit     (Assertion, assertBool, testCase, (@?=))
import           Text.Newline         (Newline (LF), newlineString)
import           Text.Parser.Char     (CharParsing)
import           Text.Parsec          (ParseError, parse)

import           Data.CSV.CSV         (CSV, mkCsv', final, noFinal, FinalRecord, singleFinal)
import           Data.CSV.Field       (Field (QuotedF, UnquotedF), MonoField (MonoField, getField))
import           Data.CSV.Parser      (comma, ending, field, pipe, doubleQuotedField, record, separatedValues, singleQuotedField)
import           Data.CSV.Record      (Record (Record), NonEmptyRecord (SingleFieldNER))
import           Data.NonEmptyString  (NonEmptyString)
import           Data.Separated       (sprinkle)
import           Text.Between         (betwixt, uniform)
import           Text.Escaped         (Escaped (SeparatedByEscapes), noEscape)
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
uq :: s -> MonoField spc s
uq = MonoField . UnquotedF
uqa :: NonEmpty s -> Record spc s
uqa = Record . fmap uq
uqaa :: [NonEmpty s] -> [Record spc s]
uqaa = fmap uqa
nospc :: Quoted s2 -> Field Text s1 s2
nospc = QuotedF . uniform ""

quotedFieldTest :: (forall m . CharParsing m => m (Field Text Text Text)) -> TestName -> Quote -> TestTree
quotedFieldTest parser name quote =
  let p = parse parser "" . concat
      q = [quoteChar quote]
      qq = Quoted quote . noEscape
  in testGroup name [
    testCase "empty" $
      p [q,q] @?=/ nospc (qq "")
  , testCase "text" $
      p [q,"hello text",q]
        @?=/ nospc (qq "hello text")
  , testCase "capture space" $
      p ["   ", q, " spaced text  ", q, "     "]
        @?=/ QuotedF (betwixt "   " "     " (qq " spaced text  "))
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  , testCase "quoted field can handle escaped quotes" $
     p [q,"yes", q, q, "no", q] @?=/ nospc (Quoted quote (SeparatedByEscapes ("yes" :| ["no"])))
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" SingleQuote
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" DoubleQuote

fieldTest :: TestTree
fieldTest =
  let p :: Text -> Either ParseError (Field Text Text Text)
      p = parse (field comma) ""
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ nospc (qd "hello")
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ nospc (qs "goodbye")
  , testCase "unquoted" $
      p "yes" @?=/ getField (uq "yes")
  , testCase "spaced doublequoted" $
     p "       \" spaces  \"    " @?=/ QuotedF (betwixt "       " "    " (qd " spaces  "))
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ QuotedF (betwixt "        " " " (qs " more spaces "))
  , testCase "spaced unquoted" $
     p "  text  " @?=/ getField (uq "  text  ")
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ nospc (qs "hello,there,")
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ nospc (qd "court,of,the,,,,crimson,king")
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ getField (uq "close")
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ getField (uq "")
  ]

recordTest :: TestTree
recordTest =
  let p :: Text -> Either ParseError (Record Text Text)
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

sqf :: Text -> FinalRecord Text b Text
sqf = final . SingleFieldNER . QuotedF . betwixt "" "" . qs
uqf :: NonEmptyString -> FinalRecord a Text1 b
uqf = final . SingleFieldNER . UnquotedF . (^. packed1)

finalRecordTest :: TestTree
finalRecordTest =
  let p :: Text -> Either ParseError (FinalRecord Text Text1 Text)
      p = parse (ending comma) ""
  in  testGroup "finalRecord" [
    testCase "single character" $
      p "a" @?=/ uqf ('a':|[])
  , testCase "empty string" $
      p "''" @?=/ sqf ""
  ]

separatedValuesTest :: Char -> Newline -> TestTree
separatedValuesTest sep nl =
  let p :: Text -> Either ParseError (CSV Text Text1 Text)
      p = parse (separatedValues sep) ""
      ps = parse (separatedValues sep) "" . concat
      csv :: [Record spc s2] -> FinalRecord spc s1 s2 -> CSV spc s1 s2
      csv rs e = mkCsv' sep e $ sprinkle nl rs
      s = [sep]
      nls = newlineString nl
  in  testGroup "separatedValues" [
    testCase "empty" $
      p "" @?=/ csv [] noFinal
  , testCase "single empty quotes field" $ 
      p "''" @?=/ csv [] (sqf "")
  , testCase "single field, single record" $
      p "one" @?=/ csv [] (singleFinal 'o' "ne")
  , testCase "single field, multiple records" $
      p "one\nun" @?=/ csv [uqa (pure "one")] (singleFinal 'u' "n")
  , testCase "multiple fields, single record" $
      ps ["one", s, "two", nls] @?=/ csv (uqaa (pure ("one":|["two"]))) noFinal
  , testCase "multiple fields, multiple records" $
      ps ["one", s, "two", s, "three", nls, "un", s, "deux", s, "trois", nls]
        @?=/ csv (uqaa ["one":|["two", "three"] , "un":|["deux", "trois"]]) noFinal
  ]

csvTest, psvTest :: TestTree
csvTest = separatedValuesTest comma LF
psvTest = separatedValuesTest pipe LF

