{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.Csv.Decode.Type (
  FieldDecode (..)
, DecodeState (..)
, RowDecode (..)
, DecodeValidation
, DecodeError (..)
, DecodeErrors (..)
) where

import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.State (MonadState, State, runState, state)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty
import Data.Semigroup
import Data.Validation (AccValidation (AccSuccess, AccFailure))

import Data.Csv.Field (Field)
import Data.Csv.Record (Record)

newtype FieldDecode e s a =
  FieldDecode { unwrapFieldDecode :: Compose (DecodeState s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

instance Alt (FieldDecode e s) where
  FieldDecode (Compose as) <!> FieldDecode (Compose bs) =
    FieldDecode . Compose . DecodeState . state $ \fs ->
      case runState (getDecodeState as) fs of
        (a, gs) -> case runState (getDecodeState bs) fs of
          (b, hs) ->
            let a' = fmap (,gs) a
                b' = fmap (,hs) b
            in  case a' <!> b' of
                  AccFailure e -> (AccFailure e, hs)
                  AccSuccess (z,js) -> (AccSuccess z, js)

newtype DecodeState s a =
  DecodeState { getDecodeState :: State [Field s] a }
  deriving (Functor, Apply, Applicative, Monad, MonadState [Field s])

newtype RowDecode e s a =
  RowDecode { unwrapRowDecode :: ReaderT (Record s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

-- TODO eventually give this type a much better show
data DecodeError e =
  UnexpectedEndOfRow
  | ExpectedEndOfRow [Field e]
  | UnknownCanonicalValue e [(e, [e])]
  | BadParse e
  | BadDecode e
  deriving (Eq, Ord, Show)

-- TODO give this a field accessor?
-- TODO use something faster than a NEL?
newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup)

type DecodeValidation e = AccValidation (DecodeErrors e)
