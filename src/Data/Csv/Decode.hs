{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Decode where

import Control.Lens (view)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.State (MonadIO, MonadState, State, runState, state)
import Data.Foldable (toList)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (rmapC)
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, bindValidation)

import Data.Csv.Csv (Csv, records)
import Data.Csv.Decode.Error (DecodeError, DecodeValidation, decodeError, expectedEndOfRow, unexpectedEndOfRow)
import Data.Csv.Field (Field, FieldContents, contents)
import Data.Csv.Parser (separatedValues)
import Data.Csv.Record (Record, _fields)
import Data.List.NonEmpty.Extra (AsNonEmpty)
import Text.Babel (Textual, IsString1, toByteString, retext)
import Text.Trifecta

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

fieldDecode :: (FieldContents s, Semigroup e) => (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . contents)

contentsD :: (FieldContents s, Semigroup e) => FieldDecode e s s
contentsD = fieldDecode AccSuccess

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

(<&>) :: (Textual s, Textual e, Semigroup e) => FieldDecode e s (a -> b) -> FieldDecode e s a -> RowDecode e s b
(<&>) ab a = row (ab <*> a)
infixl 4 <&>

decodeCsv :: AsNonEmpty t s => RowDecode e s a -> Csv t s -> DecodeValidation e [a]
decodeCsv r = traverse (runRowDecode r) . records

parseDecode ::
  (Textual s, AsNonEmpty t s, IsString1 t)
  => RowDecode e s a
  -> s
  -> Result (DecodeValidation e [a])
parseDecode d =
  fmap (decodeCsv d) . parseByteString (separatedValues ',') mempty . toByteString

fileDecode :: (MonadIO m, AsNonEmpty t s, Textual s, IsString1 t) => RowDecode e s a -> FilePath -> m (Result (DecodeValidation e [a]))
fileDecode d =
  fmap (fmap (decodeCsv d)) . parseFromFileEx (separatedValues ',')
