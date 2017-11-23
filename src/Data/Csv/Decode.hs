{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Csv.Decode where

import Control.Lens (view)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), withReaderT)
import Control.Monad.State (MonadState, State, runState, state)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (injl, injr, rmapC)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, Validate, bindValidation)

import Data.Csv.Decode.Error (DecodeValidation, badDecode)
import Data.Csv.Field (Field, FieldContents, contents)
import Data.Csv.Record (Record)

type Decode = DecodeT Identity

newtype DecodeT f e s a =
  DecodeT { runDecodeT :: ReaderT s (Compose f (AccValidation e)) a }
  deriving (Functor, Applicative)

instance (Apply f, Semigroup e) => Apply (DecodeT f e s) where
  DecodeT f <.> DecodeT a = DecodeT (f <.> a)

instance Functor f => Profunctor (DecodeT f e) where
  dimap f g (DecodeT d) = DecodeT (withReaderT f (fmap g d))

runDecode :: Decode e s a -> s -> AccValidation e a
runDecode (DecodeT r) = fmap (runIdentity . getCompose) (runReaderT r)

newtype FieldDecode e s a =
  FieldDecode { runFieldDecode :: DecodeT (DecodeState s) e (Field s) a }
  deriving (Functor, Apply, Applicative)

instance Alt (FieldDecode e s) where
  FieldDecode (DecodeT d1) <!> FieldDecode (DecodeT d2) =
    FieldDecode . DecodeT . ReaderT $ \s -> Compose $
      let as = getCompose (runReaderT d1 s)
          bs = getCompose (runReaderT d2 s)
      in  decodeState $ \fs ->
            case runDecodeState as fs of
              (a, gs) -> case runDecodeState bs fs of
                (b, hs) ->
                  let a' = fmap (,gs) a
                      b' = fmap (,hs) b
                  in  case a' <!> b' of
                        AccFailure e -> (AccFailure e, hs)
                        AccSuccess (z,js) -> (AccSuccess z, js)

decoder :: (Applicative f, Validate v) => (s -> v e a) -> DecodeT f e s a
decoder = DecodeT . ReaderT . fmap (Compose . pure . view _AccValidation)

failure :: Applicative f => e -> DecodeT f e s a
failure = decoder . const . AccFailure

success :: Applicative f => a -> DecodeT f e s a
success = decoder . const . AccSuccess

(==<<) :: (a -> AccValidation e b) -> FieldDecode e s a -> FieldDecode e s b
(==<<) f (FieldDecode (DecodeT (ReaderT s))) =
  FieldDecode (DecodeT (ReaderT (fmap (rmapC (flip bindValidation (view _AccValidation . f))) s)))

infixr 1 ==<<

(>>==) :: FieldDecode e s a -> (a -> AccValidation e b) -> FieldDecode e s b
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
drop1D = FieldDecode . DecodeT . ReaderT $ (const (injl drop1))

fieldDecode_ :: Validate v => (Field s -> v e a) -> FieldDecode e s a
fieldDecode_ f = FieldDecode $ DecodeT $ ReaderT $ injr . view _AccValidation . f

fieldDecode :: FieldContents s => (Semigroup e, Validate v) => (s -> v e a) -> FieldDecode e s a
fieldDecode f = fieldDecode_ (f . contents) <* drop1D

contentsD :: (FieldContents s, Semigroup e) => FieldDecode e s s
contentsD = fieldDecode AccSuccess

decodeMay :: (a -> Maybe b) -> e -> a -> DecodeValidation e b
decodeMay ab e a = decodeMay' e (ab a)

decodeMay' :: e -> Maybe b -> DecodeValidation e b
decodeMay' e = maybe (badDecode e) pure
