{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.Decode (
  FieldDecode (FieldDecode, unwrapFieldDecode)
, runFieldDecode
, (==<<)
, (>>==)
, DecodeState (DecodeState, getDecodeState)
, decodeState
, runDecodeState
, fieldDecode
, fieldDecode_
, decodeMay
, decodeMay'
, RowDecode (RowDecode, unwrapRowDecode)
, rowDecode
, runRowDecode
, row
, (<&>)
, decode
, parseDecode
, decodeFromFile
, contents
, byteString
, utf8
, lazyByteString
, string
, lazyText
, trimmed
, ignore
, replace
, unit
, int
, integer
, float
, double
, choice
, choiceE
, orElse
, withDefault
, categorical
, decodeRead
, decodeRead'
, decodeReadWithMsg
) where

import Control.Lens (view, alaf)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.State (MonadIO, MonadState, State, runState, state)
import Data.Bifunctor (bimap, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (rmapC)
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First))
import Data.Readable (Readable (fromBS))
import Data.Set (Set, fromList, member)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, bindValidation)
import Text.Trifecta (Result, parseByteString, parseFromFileEx)

import Data.Csv.Csv (Csv, Headedness, records)
import Data.Csv.Decode.Error (DecodeError (UnknownCanonicalValue), DecodeValidation, badDecode, decodeError, expectedEndOfRow, unexpectedEndOfRow, resultToDecodeError)
import Data.Csv.Field (Field, FieldContents, fieldContents)
import Data.Csv.Parser (separatedValues)
import Data.Csv.Record (Record, _fields)
import Data.List.NonEmpty.Extra (AsNonEmpty)
import Text.Babel (Textual, IsString1, retext, showT, toByteString, toLazyByteString, toString, toText, trim)

newtype FieldDecode e s a =
  FieldDecode { unwrapFieldDecode :: Compose (DecodeState s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

instance Alt (FieldDecode e s) where
  FieldDecode (Compose as) <!> FieldDecode (Compose bs) =
    FieldDecode . Compose . decodeState $ \fs ->
      case runDecodeState as fs of
        (a, gs) -> case runDecodeState bs fs of
          (b, hs) ->
            let a' = fmap (,gs) a
                b' = fmap (,hs) b
            in  case a' <!> b' of
                  AccFailure e -> (AccFailure e, hs)
                  AccSuccess (z,js) -> (AccSuccess z, js)

runFieldDecode :: FieldDecode e s a -> [Field s] -> (DecodeValidation e a, [Field s])
runFieldDecode = runDecodeState . getCompose . unwrapFieldDecode

(==<<) :: (a -> DecodeValidation e b) -> FieldDecode e s a -> FieldDecode e s b
(==<<) f (FieldDecode c) =
  FieldDecode (rmapC (flip bindValidation (view _AccValidation . f)) c)

infixr 1 ==<<

(>>==) :: FieldDecode e s a -> (a -> DecodeValidation e b) -> FieldDecode e s b
(>>==) = flip (==<<)

infixl 1 >>==

newtype DecodeState s a =
  DecodeState { getDecodeState :: State [Field s] a }
  deriving (Functor, Apply, Applicative, Monad, MonadState [Field s])

decodeState :: ([Field s] -> (a, [Field s])) -> DecodeState s a
decodeState = DecodeState . state

runDecodeState :: DecodeState s a -> [Field s] -> (a, [Field s])
runDecodeState = runState . getDecodeState

fieldDecode_ :: (Field s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode_ f = FieldDecode . Compose . state $ \l ->
  case l of
    [] -> (unexpectedEndOfRow, [])
    (x:xs) -> (f x, xs)

fieldDecode :: FieldContents s => (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . fieldContents)

decodeMay :: (a -> Maybe b) -> DecodeError e -> a -> DecodeValidation e b
decodeMay ab e a = decodeMay' e (ab a)

decodeMay' :: DecodeError e -> Maybe b -> DecodeValidation e b
decodeMay' e = maybe (decodeError e) pure

newtype RowDecode e s a =
  RowDecode { unwrapRowDecode :: ReaderT (Record s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

rowDecode :: (Record s -> DecodeValidation e a) -> RowDecode e s a
rowDecode = RowDecode . ReaderT

runRowDecode :: RowDecode e s a -> Record s -> DecodeValidation e a
runRowDecode = runReaderT . unwrapRowDecode

row :: (Textual e, Textual s) => FieldDecode e s a -> RowDecode e s a
row a = rowDecode $ \rs ->
  case runFieldDecode a . toList . _fields $ rs of
    (v, []) -> v
    (v, xs@(_:_)) -> v *> expectedEndOfRow (fmap (fmap retext) xs)

(<&>) :: (Textual s, Textual e) => FieldDecode e s (a -> b) -> FieldDecode e s a -> RowDecode e s b
(<&>) ab a = row (ab <*> a)
infixl 4 <&>

decode :: AsNonEmpty t s => RowDecode e s a -> Csv t s -> DecodeValidation e [a]
decode r = traverse (runRowDecode r) . records

parseDecode ::
  (Textual s, AsNonEmpty t s, IsString1 t)
  => RowDecode e s a
  -> Headedness
  -> s
  -> Result (DecodeValidation e [a])
parseDecode d h =
  fmap (decode d) . parseByteString (separatedValues ',' h) mempty . toByteString

decodeFromFile ::
  (MonadIO m, Textual e, Textual s, IsString1 t, AsNonEmpty t s)
  => RowDecode e s a
  -> Headedness
  -> FilePath
  -> m (DecodeValidation e [a])
decodeFromFile d h fp =
  let decodeResult r = resultToDecodeError r `bindValidation` decode d
  in  decodeResult <$> parseFromFileEx (separatedValues ',' h) fp

contents :: FieldContents s => FieldDecode e s s
contents = fieldDecode AccSuccess

byteString :: FieldContents s => FieldDecode e s ByteString
byteString = toByteString <$> contents

utf8 :: IsString e => FieldDecode e ByteString Text
utf8 = contents >>==
  either (badDecode . fromString . show) pure . decodeUtf8'

lazyByteString :: FieldContents s => FieldDecode e s LBS.ByteString
lazyByteString = toLazyByteString <$> contents

string :: FieldContents s => FieldDecode e s String
string = toString <$> contents

lazyText :: IsString e => FieldDecode e ByteString LT.Text
lazyText = LT.fromStrict <$> text

text :: FieldContents s => FieldDecode e s Text
text = toText <$> contents

trimmed :: FieldContents s => FieldDecode e s s
trimmed = trim <$> contents

ignore :: FieldContents s => FieldDecode e s ()
ignore = () <$ contents

replace :: FieldContents s => a -> FieldDecode e s a
replace a = a <$ contents

unit :: FieldContents s => FieldDecode e s ()
unit = ignore

int :: (FieldContents s, Textual e) => FieldDecode e s Int
int = named "int"

integer :: (FieldContents s, Textual e) => FieldDecode e s Integer
integer = named "integer"

float :: (FieldContents s, Textual e) => FieldDecode e s Float
float = named "float"

double :: (FieldContents s, Textual e) => FieldDecode e s Double
double = named "double"

choice :: FieldDecode e s a -> FieldDecode e s a -> FieldDecode e s a
choice = (<!>)

choiceE :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
choiceE a b = fmap Left a <!> fmap Right b

orElse :: FieldContents s => FieldDecode e s a -> a -> FieldDecode e s a
orElse f a = f <!> replace a

withDefault :: FieldContents s => FieldDecode e s b -> a -> FieldDecode e s (Either a b)
withDefault b a = swapE <$> choiceE b (replace a)
  where
    swapE = either Right Left

categorical :: forall a s e. (FieldContents s, Ord s, Textual e, Show a) => [(a, [s])] -> FieldDecode e s a
categorical as =
  let as' :: [(a, Set s)]
      as' = fmap (second fromList) as
      go :: s -> (a, Set s) -> Maybe a
      go s (a, set) =
        if s `member` set
        then Just a
        else Nothing
  in  contents >>== \s ->
  decodeMay' (UnknownCanonicalValue (retext s) (fmap (bimap showT (fmap retext)) as)) $
    alaf First foldMap (go s) as'

decodeRead :: (Readable a, FieldContents s, Textual e) => FieldDecode e s a
decodeRead = decodeReadWithMsg (mappend "Couldn't parse " . retext)

decodeRead' :: (Textual e, Readable a) => e -> FieldDecode e ByteString a
decodeRead' e = decodeReadWithMsg (const e)

decodeReadWithMsg :: (FieldContents s, Textual e, Readable a) => (s -> e) -> FieldDecode e s a
decodeReadWithMsg e = trimmed >>== \c ->
  maybe (badDecode (e c)) pure . fromBS . toByteString $ c

named :: (Readable a, FieldContents s, Textual e) => s -> FieldDecode e s a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap n . listToMaybe
      n'' = fromString (n' (toString name))
      space = " "
  in  decodeReadWithMsg $ \bs ->
        mconcat (["Couldn't parse \"", retext bs, "\" as a", n'', space, retext name])
