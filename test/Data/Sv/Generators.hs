{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Generators (
  genSv
  , genSvWithHeadedness
  , genNewline
  , genSep
  , genQuote
  , genSpaced
  , genField
  , genSpacedField
  , genRecord
  , genRecords
  , genHeader
  , genCsvString
) where

import Control.Applicative ((<$>), liftA2, liftA3)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup ((<>)))
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Sv.Syntax.Sv     (Sv (Sv), Header (Header), Headedness, getHeadedness, Separator)
import Data.Sv.Syntax.Field  (Field (Quoted, Unquoted))
import Data.Sv.Syntax.Record (Record, Records (EmptyRecords, Records), recordNel)
import Text.Escape         (Unescaped (Unescaped))
import Text.Newline        (Newline (CRLF, LF))
import Text.Space          (Spaces, Spaced (Spaced))
import Text.Quote          (Quote (SingleQuote, DoubleQuote))

genSv :: Gen Separator -> Gen Spaces -> Gen s -> Gen (Sv s)
genSv sep spc s =
  let rs = genRecords spc s
      e  = Gen.list (Range.linear 0 5) genNewline
      h = Gen.maybe (genHeader spc s genNewline)
  in  Sv <$> sep <*> h <*> rs <*> e

genSvWithHeadedness :: Gen Separator -> Gen Spaces -> Gen s -> Gen (Sv s, Headedness)
genSvWithHeadedness sep spc s = fmap (\c -> (c, getHeadedness c)) (genSv sep spc s)

genNewline :: Gen Newline
genNewline =
  -- TODO put CR back in
  Gen.element [CRLF, LF]

genSep :: Gen Separator
genSep =
  Gen.element ['|', ',', '\t', '\x1F4A9']

genSpaced :: Gen Spaces -> Gen s -> Gen (Spaced s)
genSpaced spc str =
  liftA3 Spaced spc spc str

genQuote :: Gen Quote
genQuote =
  Gen.element [SingleQuote, DoubleQuote]

genUnescaped :: Gen a -> Gen (Unescaped a)
genUnescaped = fmap Unescaped

genField :: Gen s -> Gen (Field s)
genField s =
  Gen.choice [
    Unquoted <$> s
  , liftA2 Quoted genQuote (genUnescaped s)
  ]

genSpacedField :: Gen Spaces -> Gen s -> Gen (Spaced (Field s))
genSpacedField spc s = genSpaced spc (genField s)

genRecord :: Gen Spaces -> Gen s -> Gen (Record s)
genRecord spc s =
  recordNel <$> Gen.nonEmpty (Range.linear 1 10) (genSpacedField spc s)

genHeader :: Gen Spaces -> Gen s -> Gen Newline -> Gen (Header s)
genHeader spc s n =
  Header <$> genRecord spc s <*> n

genRecords :: Gen Spaces -> Gen s -> Gen (Records s)
genRecords spc s =
  let rec = genRecord spc s
      nlr = liftA2 (,) genNewline rec
  in  maybe EmptyRecords (uncurry Records) <$>
    Gen.maybe (
      liftA2 (,)
        rec
        (V.fromList <$> Gen.list (Range.linear 0 1000) nlr)
      )

genCsvString :: Gen ByteString
genCsvString =
  let intercalate' :: (Semigroup m, Monoid m) => m -> NonEmpty m -> m
      intercalate' _ (x:|[]) = x
      intercalate' m (x:|y:zs) = x <> m <> intercalate' m (y:|zs)
      genNewlineString :: Gen Builder
      genNewlineString = Gen.element (fmap Builder.string7 ["\n", "\r", "\r\n"])
      genCsvRowString = intercalate' "," <$> Gen.nonEmpty (Range.linear 1 100) genCsvField
      enquote c s = fmap (\z -> c <> z <> c) s
      genCsvFieldString :: Gen Builder
      genCsvFieldString = Builder.byteString <$>
        Gen.utf8 (Range.linear 1 50) (Gen.filter (`notElem` [',','"','\'','\n','\r']) Gen.unicode)
      genCsvField =
        Gen.choice [
          enquote "\"" genCsvFieldString
        , enquote "'" genCsvFieldString
        , genCsvFieldString
        ]
  in  fmap (LBS.toStrict . Builder.toLazyByteString) $ intercalate' <$> genNewlineString <*> Gen.nonEmpty (Range.linear 0 100) genCsvRowString
