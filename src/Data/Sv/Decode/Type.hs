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
, buildDecode
, DecodeState (..)
, runDecodeState
, Ind (Ind)
, DecodeError (..)
, DecodeErrors (..)
, DecodeValidation
, Validation (Success, Failure)
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
-- type 'e' of error strings. Usually you want 'e' to be the same as 's'
-- (as in 'Decode'')
--
-- There are primitive 'Decode's, and combinators for composing or
-- otherwise manipulating them. In particular, 'Decode' is an
-- 'Applicative' functor and an 'Alt'.
--
-- 'Decode' is not a 'Monad', but we can perform monad-like operations on
-- it with 'Data.Sv.Decode.Field.>>==' and 'Data.Sv.Decode.bindDecode'
newtype Decode e s a =
  Decode { unwrapDecode :: Compose (DecodeState s) (DecodeValidation e) a }
  deriving (Functor, Apply, Applicative)

-- | 'Decode'' is 'Decode' with the input and error types the same. You usually
-- want them to be the same, and most primitives are set up this way.
type Decode' s = Decode s s

instance Alt (Decode e s) where
  Decode (Compose as) <!> Decode (Compose bs) =
    buildDecode $ \v i ->
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

-- | Convenient constructor for 'Decode' that handles all the newtype noise for you.
buildDecode :: (Vector (SpacedField s) -> Ind -> (DecodeValidation e a, Ind)) -> Decode e s a
buildDecode f = Decode . Compose . DecodeState . ReaderT $ \v -> state $ \i -> f v i

-- | Convenient function to run a DecodeState
runDecodeState :: DecodeState s a -> Vector (SpacedField s) -> Ind -> (a, Ind)
runDecodeState = fmap runState . runReaderT . getDecodeState

-- | Newtype for indices into the field vector
newtype Ind = Ind Int

-- | 'DecodeError' is a value indicating what went wrong during a parse or
-- decode. Its constructor indictates the type of error which occured, and
-- there is usually an associated string with more finely-grained details.
data DecodeError e =
  -- | I was looking for another field, but I am at the end of the row
  UnexpectedEndOfRow
  -- | I should be at the end of the row, but I found extra fields
  | ExpectedEndOfRow (Vector (SpacedField e))
  -- | This decoder was built using the 'categorical' primitive for categorical data
  | UnknownCategoricalValue e [[e]]
  -- | The parser failed, meaning decoding proper didn't even begin
  | BadParse e
  -- | Some other kind of decoding failure occured
  | BadDecode e
  deriving (Eq, Ord, Show, Generic)

instance Functor DecodeError where
  fmap f d = case d of
    UnexpectedEndOfRow -> UnexpectedEndOfRow
    ExpectedEndOfRow v -> ExpectedEndOfRow (fmap (fmap (fmap f)) v)
    UnknownCategoricalValue e ess -> UnknownCategoricalValue (f e) (fmap (fmap f) ess)
    BadParse e -> BadParse (f e)
    BadDecode e -> BadDecode (f e)

instance NFData e => NFData (DecodeError e)

-- | 'DecodeErrors' is a 'Semigroup' full of 'DecodeError'. It is used as the
-- error side of a 'DecodeValidation'. When multiple errors occur, they will
-- be collected.
newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup, Generic)

instance Functor DecodeErrors where
  fmap f (DecodeErrors nel) = DecodeErrors (fmap (fmap f) nel)

instance NFData e => NFData (DecodeErrors e)

-- | 'DecodeValidation' is the error-accumulating 'Applicative' underlying
-- 'Decode'
type DecodeValidation e = Validation (DecodeErrors e)
