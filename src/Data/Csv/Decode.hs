{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Csv.Decode where

import Control.Applicative (Applicative, Alternative, liftA2)
import Control.Lens (view)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), withReaderT)
import Control.Monad.State (MonadState, State, runState, state)
import Data.Csv.Field (Field)
import Data.Csv.Record (Record)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, Validate)

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
  FieldDecode (DecodeT a) <!> FieldDecode (DecodeT b) =
    FieldDecode . DecodeT . ReaderT $ \s -> Compose $
      let as = getCompose (runReaderT a s)
          bs = getCompose (runReaderT b s)
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


newtype DecodeState s a =
  DecodeState { getDecodeState :: State [Field s] a }
  deriving (Functor, Apply, Applicative, Monad, MonadState [Field s])

decodeState :: ([Field s] -> (a, [Field s])) -> DecodeState s a
decodeState = DecodeState . state

runDecodeState :: DecodeState s a -> [Field s] -> (a, [Field s])
runDecodeState = runState . getDecodeState

drop1 :: DecodeState s ()
drop1 = DecodeState (state $ \l -> ((), drop 1 l))

