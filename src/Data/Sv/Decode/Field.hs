{-|
Module      : Data.Sv.Decode.Field
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.Field (
  Decode (..)
, runDecode
, (>>==)
, (==<<)
, fieldDecode
, fieldDecodeWithQuotes
, fieldDecodeWithSpaces
, validateMay
, validateMay'
, promote
) where


import Control.Lens (view)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.State (state)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Validation (_Validation, bindValidation)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Sv.Decode.Error
import Data.Sv.Decode.Type
import Data.Sv.Syntax.Field (Field, SpacedField, fieldContents)
import Data.Sv.Syntax.Record (Record, _fields)
import Text.Space (Spaced (_value))

-- | Convenience to get the underlying function out of a Decode in a useful form
runDecode :: Decode e s a -> Vector (SpacedField s) -> Ind -> (DecodeValidation e a, Ind)
runDecode = runDecodeState . getCompose . unwrapDecode

-- | This can be used to build a 'Decode' whose value depends on the
-- result of another 'Decode'. This is especially useful since
(>>==) :: Decode e s a -> (a -> DecodeValidation e b) -> Decode e s b
(>>==) = flip (==<<)
infixl 1 >>==
{-# INLINE (>>==) #-}

-- | flipped '(>>==)''
(==<<) :: (a -> DecodeValidation e b) -> Decode e s a -> Decode e s b
(==<<) f (Decode c) =
  Decode (rmapC (`bindValidation` (view _Validation . f)) c)
    where
      rmapC g (Compose fga) = Compose (fmap g fga)
infixr 1 ==<<

-- | Build a 'Decode' from a function.
--
-- This version gives you just the contents of the field, with no information
-- about the spacing or quoting around that field.
fieldDecode :: (s -> DecodeValidation e a) -> Decode e s a
fieldDecode f = fieldDecodeWithQuotes (f . view fieldContents)

-- | Build a 'Decode' from a function.
--
-- This version gives you access to the whole 'Field', which includes
-- information about whether quotes were used, and if so which ones.
fieldDecodeWithQuotes :: (Field s -> DecodeValidation e a) -> Decode e s a
fieldDecodeWithQuotes f = fieldDecodeWithSpaces (f . _value)

-- | Build a 'Decode' from a function.
--
-- This version gives you access to the whole 'SpacedField', which includes
-- information about spacing both before and after the field, and about quotes
-- if they were used.
fieldDecodeWithSpaces :: (SpacedField s -> DecodeValidation e a) -> Decode e s a
fieldDecodeWithSpaces f =
  Decode . Compose . DecodeState . ReaderT $ \v -> state $ \(Ind i) ->
    if i >= length v
    then (unexpectedEndOfRow, Ind i)
    else (f (v ! i), Ind (i+1))

-- | promotes a Decode to work on a whole 'Record' at once
promote :: Decode' s a -> Record s -> DecodeValidation s a
promote dec rs =
  let vec = V.fromList . toList . _fields $ rs
      len = length vec
  in  case runDecode dec vec (Ind 0) of
    (d, Ind i) ->
      if i >= len
      then d
      else d *> expectedEndOfRow (V.force (V.drop i vec))
