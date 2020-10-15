{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Data.Sv.Decode.Type
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.Type (
  Decode (..)
, Decode'
, buildDecode
, NameDecode (..)
, NameDecode'
, DecodeState (..)
, runDecodeState
, Ind (..)
, DecodeError (..)
, DecodeErrors (..)
, DecodeValidation
, Validation (..)
) where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader, withReaderT)
import Control.Monad.State (State, runState, state, MonadState)
import Control.Monad.Writer (Writer, writer, runWriter)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Monoid (Last)
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroupoid (Semigroupoid (o))
import Data.Profunctor (Profunctor (lmap, rmap))
import Data.Validation (Validation (Success, Failure))
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | A @'Decode' e s a@ is for decoding some fields from a CSV row into our type 'a'.
--
-- The second type parameter (@s@) is the input string type
-- (usually 'ByteString' or 'Text').
-- The first type parameter (@e@) is the type of strings which occur in errors.
-- Under most circumstances you want these type parameters to coincide, but they
-- don't have to. They are two separate type parameters instead of one so that
-- 'Decode' can have a 'Data.Profunctor.Profunctor' instance.
--
-- There are primitive 'Decode's, and combinators for composing or
-- otherwise manipulating them. In particular, 'Decode' is an
-- 'Applicative' functor and an 'Alt' from the semigroupoids package, also known
-- as a @SemiAlternative@.
--
-- 'Decode' is not a 'Monad', but we can perform monad-like operations on
-- it with 'Data.Sv.Decode.>>==' or 'Data.Sv.Decode.bindDecode'
newtype Decode e s a =
  Decode { unwrapDecode :: Compose (DecodeState s) (Compose (Writer (Last Bool)) (DecodeValidation e)) a }
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
            in  case runWriter $ liftA2 (<!>) (getCompose a') (getCompose b') of
                  (Failure e, l) -> (Failure e, l, k)
                  (Success (z, m), l) -> (Success z, l, m)

instance Profunctor (Decode e) where
  lmap f (Decode (Compose dec)) = Decode (Compose (lmap f dec))
  rmap = fmap

instance Semigroupoid (Decode e) where
  r `o` s = case r of
    Decode (Compose (DecodeState (ReaderT r'))) -> case s of
      Decode (Compose (DecodeState (ReaderT s'))) ->
        buildDecode $ \vec ind -> case runState (s' vec) ind of
            (v,ind') -> case runWriter (getCompose v) of
              (Failure e, l) -> (Failure e, l, ind')
              (Success x, l) ->
                case runWriter $ getCompose $ fst (runState (r' (pure x)) (Ind 0)) of
                  (y, l') -> (y, l <> l', ind')

-- | As we decode a row of data, we walk through its fields. This 'Monad'
-- keeps track of our position.
newtype DecodeState s a =
  DecodeState { getDecodeState :: ReaderT (Vector s) (State Ind) a }
  deriving (Functor, Apply, Applicative, Monad, MonadReader (Vector s), MonadState Ind)

instance Bind (DecodeState s) where
  (>>-) = (>>=)

instance Profunctor DecodeState where
  lmap f (DecodeState s) = DecodeState (withReaderT (fmap f) s)
  rmap = fmap

-- | Convenient constructor for 'Decode' that handles all the newtype noise for you.
buildDecode :: (Vector s -> Ind -> (DecodeValidation e a, Last Bool, Ind)) -> Decode e s a
buildDecode f =
  Decode . Compose . DecodeState . ReaderT $ \v -> state $ \i ->
    case f v i of
      (va, l, i') -> (Compose (writer (va, l)), i')

-- | Convenient function to run a DecodeState
runDecodeState :: DecodeState s a -> Vector s -> Ind -> (a, Ind)
runDecodeState = fmap runState . runReaderT . getDecodeState

-- | Newtype for indices into the field vector
newtype Ind = Ind Int deriving (Eq, Ord, Show)

-- | 'DecodeError' is a value indicating what went wrong during a parse or
-- decode. Its constructor indictates the type of error which occured, and
-- there is usually an associated string with more finely-grained details.
data DecodeError e =
  -- | I was looking for another field, but I am at the end of the row
  UnexpectedEndOfRow
  -- | I should be at the end of the row, but I found extra fields
  | ExpectedEndOfRow (Vector e)
  -- | This decoder was built using the 'categorical' primitive for categorical data
  | UnknownCategoricalValue e [[e]]
  -- | Looked for a column with this name, but could not find it
  | MissingColumn e
  -- | There should have been a header but there was nothing
  | MissingHeader
  -- | sv is misconfigured
  | BadConfig e
  -- | The parser failed, meaning decoding proper didn't even begin
  | BadParse e
  -- | Some other kind of decoding failure occured
  | BadDecode e
  deriving (Eq, Ord, Show, Generic)

instance Functor DecodeError where
  fmap f d = case d of
    UnexpectedEndOfRow -> UnexpectedEndOfRow
    ExpectedEndOfRow v -> ExpectedEndOfRow (fmap f v)
    UnknownCategoricalValue e ess -> UnknownCategoricalValue (f e) (fmap (fmap f) ess)
    MissingColumn e -> MissingColumn (f e)
    MissingHeader -> MissingHeader
    BadConfig e -> BadConfig (f e)
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

-- | 'NameDecode' is a decoder that looks for a column by name rather than
-- by position.
newtype NameDecode e s a =
  Named {
    unNamed :: ReaderT (Map s Ind) (Compose (DecodeValidation e) (Decode e s)) a
  }
  deriving (Functor, Applicative)

-- | 'NameDecode'' is 'NameDecode' with both type parameters the same, as
-- should usually be the case
type NameDecode' s = NameDecode s s

instance Alt (NameDecode e s) where
  Named f <!> Named g = Named (f <!> g)
