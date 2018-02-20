{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Data.Sv.Decode.Type
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.Type (
  FieldDecode (..)
, FieldDecode'
, DecodeState (..)
, runDecodeState
, Ind (Ind)
, AccValidation (AccSuccess, AccFailure)
, DecodeValidation
, DecodeError (..)
, DecodeErrors (..)
) where

import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader, withReaderT)
import Control.Monad.State (State, runState, state, MonadState)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty
import Data.Semigroup
import Data.Profunctor (Profunctor (lmap, rmap))
import Data.Validation (AccValidation (AccSuccess, AccFailure))
import Data.Vector (Vector)

import Data.Sv.Syntax.Field (SpacedField)

-- | A 'FieldDecode e s a' is for decoding some fields from a CSV row into our type 'a'.
--
-- It also knows the string input type 's' (usually 'ByteString' or 'Text') and the
-- type 'e' of error strings (usually the same as 's')
--
-- There are primitive 'FieldDecode's, and combinators for composing or
-- otherwise manipulating them. In particular, 'FieldDecode' is an
-- 'Applicative' functor and an 'Alt'. 'Alt' is 'Control.Applicative.Alternative'
-- without 'Control.Applicative.empty'
--
-- 'FieldDecode' is not a 'Monad', but we can perform monad-like operations on
-- it with 'Data.Sv.Decode.Field.>>=='
newtype FieldDecode e s a =
  FieldDecode { unwrapFieldDecode :: Compose (DecodeState s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

-- | 'FieldDecode'' is 'FieldDecode' with the input and error types the same.
type FieldDecode' s = FieldDecode s s

instance Alt (FieldDecode e s) where
  FieldDecode (Compose as) <!> FieldDecode (Compose bs) =
    FieldDecode . Compose . DecodeState . ReaderT $ \v -> state $ \i ->
      case runDecodeState as v i of
        (a, j) -> case runDecodeState bs v i of
          (b, k) ->
            let a' = fmap (,j) a
                b' = fmap (,k) b
            in  case a' <!> b' of
                  AccFailure e -> (AccFailure e, k)
                  AccSuccess (z, m) -> (AccSuccess z, m)

instance Profunctor (FieldDecode e) where
  lmap f (FieldDecode (Compose dec)) = FieldDecode (Compose (lmap f dec))
  rmap = fmap

-- | As we decode a row of data, we walk through its 'Data.Sv.Syntax.Field's. This 'Monad'
-- keeps track of our remaining 'Data.Sv.Syntax.Field's.
newtype DecodeState s a =
  DecodeState { getDecodeState :: ReaderT (Vector (SpacedField s)) (State Ind) a }
  deriving (Functor, Apply, Applicative, Monad, MonadReader (Vector (SpacedField s)), MonadState Ind)

instance Bind (DecodeState s) where
  (>>-) = (>>=)

instance Profunctor DecodeState where
  lmap f (DecodeState s) = DecodeState (withReaderT (fmap (fmap (fmap f))) s)
  rmap = fmap

-- | Convenient function to run a DecodeState
runDecodeState :: DecodeState s a -> Vector (SpacedField s) -> Ind -> (a, Ind)
runDecodeState = fmap runState . runReaderT . getDecodeState

-- | Newtype for indices into the field vector
newtype Ind = Ind Int

-- TODO eventually give DecodeError a much better show

-- | 'DecodeError' is a value indicating what went wrong during a decode.
-- Its parameter type will usually be some sort of string.
data DecodeError e =
  UnexpectedEndOfRow
  | ExpectedEndOfRow (Vector (SpacedField e))
  | UnknownCanonicalValue e [[e]]
  | BadParse e
  | BadDecode e
  deriving (Eq, Ord, Show)

-- TODO give this a field accessor?
-- TODO use something faster than a NEL?

-- | 'DecodeErrors' are many 'DecodeError's
newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup)

-- | 'DecodeValidation' is the error-accumulating 'Applicative' underlying
-- 'FieldDecode'
type DecodeValidation e = AccValidation (DecodeErrors e)
