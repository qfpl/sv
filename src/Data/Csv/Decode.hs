{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Csv.Decode where

import Control.Lens (view)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), withReaderT)
import Control.Monad.State (MonadState, State, runState, state)
import Data.Foldable (toList)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (injl, injr, rmapC)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, bindValidation)

import Data.Csv.Decode.Error (DecodeError, DecodeValidation, badDecode, decodeError, expectedEndOfRow, unexpectedEndOfRow)
import Data.Csv.Field (Field, FieldContents, contents)
import Data.Csv.Record (Record, _fields)
import Data.String.Babel (Textual, retext)

type Decode = DecodeT Identity

newtype DecodeT f e s a =
  DecodeT { runDecodeT :: ReaderT s (Compose f (DecodeValidation e)) a }
  deriving (Functor, Applicative)

instance (Apply f, Semigroup e) => Apply (DecodeT f e s) where
  DecodeT f <.> DecodeT a = DecodeT (f <.> a)

instance Functor f => Profunctor (DecodeT f e) where
  dimap f g (DecodeT d) = DecodeT (withReaderT f (fmap g d))

runDecode :: Decode e s a -> s -> DecodeValidation e a
runDecode (DecodeT r) = fmap (runIdentity . getCompose) (runReaderT r)

decoder :: Applicative f => (s -> DecodeValidation e a) -> DecodeT f e s a
decoder = DecodeT . ReaderT . fmap (Compose . pure . view _AccValidation)

failure :: Applicative f => DecodeError e -> DecodeT f e s a
failure = decoder . const . decodeError

success :: Applicative f => a -> DecodeT f e s a
success = decoder . const . AccSuccess

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

drop1 :: DecodeState s ()
drop1 = DecodeState (state $ \l -> ((), drop 1 l))

drop1D :: Semigroup e => FieldDecode e s ()
drop1D = FieldDecode (injl drop1)

fieldDecode_ :: (Field s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode_ f = FieldDecode . Compose . state $ \l ->
  case l of
    [] -> (unexpectedEndOfRow, [])
    (x:xs) -> (f x, xs)

fieldDecode :: (FieldContents s, Semigroup e) => (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . contents) <* drop1D

contentsD :: (FieldContents s, Semigroup e) => FieldDecode e s s
contentsD = fieldDecode AccSuccess

decodeMay :: (a -> Maybe b) -> e -> a -> DecodeValidation e b
decodeMay ab e a = decodeMay' e (ab a)

decodeMay' :: e -> Maybe b -> DecodeValidation e b
decodeMay' e = maybe (badDecode e) pure

newtype RowDecode e s a =
  RowDecode { runRowDecode :: DecodeT Identity e (Record s) a }
  deriving (Functor, Applicative)

rowDecode :: (Record s -> DecodeValidation e a) -> RowDecode e s a
rowDecode = RowDecode . DecodeT . ReaderT . fmap injr

row :: (Textual e, Textual s) => FieldDecode e s a -> RowDecode e s a
row a = rowDecode $ \rs ->
  case runFieldDecode a . toList . _fields $ rs of
    (v, []) -> v
    (v, xs@(_:_)) -> v *> expectedEndOfRow (fmap (fmap retext) xs)

(<&>) :: (Textual s, Textual e, Semigroup e) => FieldDecode e s (a -> b) -> FieldDecode e s a -> RowDecode e s b
(<&>) ab a = row (ab <*> a)
