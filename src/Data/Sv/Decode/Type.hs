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
, module Data.Validation
, DecodeValidation
, DecodeError (..)
, DecodeErrors (..)
) where

import Control.Monad.State (MonadState, State, runState, state)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty
import Data.Semigroup
import Data.Validation (AccValidation (AccSuccess, AccFailure))

import Data.Sv.Field (SpacedField)

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
    FieldDecode . Compose . DecodeState . state $ \fs ->
      case runState (getDecodeState as) fs of
        (a, gs) -> case runState (getDecodeState bs) fs of
          (b, hs) ->
            let a' = fmap (,gs) a
                b' = fmap (,hs) b
            in  case a' <!> b' of
                  AccFailure e -> (AccFailure e, hs)
                  AccSuccess (z,js) -> (AccSuccess z, js)

-- | As we decode a row of data, we walk through its 'Data.Sv.Field's. This 'Monad'
-- keeps track of our remaining 'Data.Sv.Field's.
newtype DecodeState s a =
  DecodeState { getDecodeState :: State [SpacedField s] a }
  deriving (Functor, Apply, Applicative, Monad, MonadState [SpacedField s])

-- TODO eventually give DecodeError a much better show

-- | 'DecodeError' is a value indicating what went wrong during a decode.
-- Its parameter type will usually be some sort of string.
data DecodeError e =
  UnexpectedEndOfRow
  | ExpectedEndOfRow [SpacedField e]
  | UnknownCanonicalValue e [(e, [e])]
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
