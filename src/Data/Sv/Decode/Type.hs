{-# LANGUAGE DeriveGeneric #-}
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
  Decode (..)
, Decode'
, DecodeState (..)
, runDecodeState
, Ind (Ind)
, Validation (Success, Failure)
, DecodeValidation
, DecodeError (..)
, DecodeErrors (..)
) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader, withReaderT)
import Control.Monad.State (State, runState, state, MonadState)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty
import Data.Semigroup
import Data.Profunctor (Profunctor (lmap, rmap))
import Data.Validation (Validation (Success, Failure))
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Data.Sv.Syntax.Field (SpacedField)

-- | A 'Decode e s a' is for decoding some fields from a CSV row into our type 'a'.
--
-- It also knows the string input type 's' (usually 'ByteString' or 'Text') and the
-- type 'e' of error strings (usually the same as 's')
--
-- There are primitive 'Decode's, and combinators for composing or
-- otherwise manipulating them. In particular, 'Decode' is an
-- 'Applicative' functor and an 'Alt'. 'Alt' is 'Control.Applicative.Alternative'
-- without 'Control.Applicative.empty'
--
-- 'Decode' is not a 'Monad', but we can perform monad-like operations on
-- it with 'Data.Sv.Decode.Field.>>=='
newtype Decode e s a =
  Decode { unwrapDecode :: Compose (DecodeState s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

-- | 'Decode'' is 'Decode' with the input and error types the same.
type Decode' s = Decode s s

instance Alt (Decode e s) where
  Decode (Compose as) <!> Decode (Compose bs) =
    Decode . Compose . DecodeState . ReaderT $ \v -> state $ \i ->
      case runDecodeState as v i of
        (a, j) -> case runDecodeState bs v i of
          (b, k) ->
            let a' = fmap (,j) a
                b' = fmap (,k) b
            in  case a' <!> b' of
                  Failure e -> (Failure e, k)
                  Success (z, m) -> (Success z, m)

instance Profunctor (Decode e) where
  lmap f (Decode (Compose dec)) = Decode (Compose (lmap f dec))
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
  deriving (Eq, Ord, Show, Generic)

instance Functor DecodeError where
  fmap f d = case d of
    UnexpectedEndOfRow -> UnexpectedEndOfRow
    ExpectedEndOfRow v -> ExpectedEndOfRow (fmap (fmap (fmap f)) v)
    UnknownCanonicalValue e ess -> UnknownCanonicalValue (f e) (fmap (fmap f) ess)
    BadParse e -> BadParse (f e)
    BadDecode e -> BadDecode (f e)

instance NFData e => NFData (DecodeError e)

-- TODO give this a field accessor?
-- TODO use something faster than a NEL?

-- | 'DecodeErrors' are many 'DecodeError's
newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup, Generic)

instance Functor DecodeErrors where
  fmap f (DecodeErrors nel) = DecodeErrors (fmap (fmap f) nel)

instance NFData e => NFData (DecodeErrors e)

-- | 'DecodeValidation' is the error-accumulating 'Applicative' underlying
-- 'Decode'
type DecodeValidation e = Validation (DecodeErrors e)
