{-|
Module      : Data.Sv.Decode.Field
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.Field (
  FieldDecode (..)
, runFieldDecode
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
import Data.Validation (_AccValidation, bindValidation)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Sv.Decode.Error
import Data.Sv.Decode.Type
import Data.Sv.Syntax.Field (Field, SpacedField, fieldContents)
import Data.Sv.Syntax.Record (Record, _fields)
import Text.Babel (Textual (retext))
import Text.Space (Spaced (_value))

-- | Convenience to get the underlying function out of a FieldDecode in a useful form
runFieldDecode :: FieldDecode e s a -> Vector (SpacedField s) -> Ind -> (DecodeValidation e a, Ind)
runFieldDecode = runDecodeState . getCompose . unwrapFieldDecode

-- | This can be used to build a 'FieldDecode' whose value depends on the
-- result of another 'FieldDecode'. This is especially useful since 
(>>==) :: FieldDecode e s a -> (a -> DecodeValidation e b) -> FieldDecode e s b
(>>==) = flip (==<<)
infixl 1 >>==
{-# INLINE (>>==) #-}

-- | flipped '(>>==)''
(==<<) :: (a -> DecodeValidation e b) -> FieldDecode e s a -> FieldDecode e s b
(==<<) f (FieldDecode c) =
  FieldDecode (rmapC (`bindValidation` (view _AccValidation . f)) c)
    where
      rmapC g (Compose fga) = Compose (fmap g fga)
infixr 1 ==<<

-- | Build a 'FieldDecode' from a function.
--
-- This version gives you just the contents of the field, with no information
-- about the spacing or quoting around that field.
fieldDecode :: (s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecode f = fieldDecodeWithQuotes (f . view fieldContents)

-- | Build a 'FieldDecode' from a function.
--
-- This version gives you access to the whole 'Field', which includes
-- information about whether quotes were used, and if so which ones.
fieldDecodeWithQuotes :: (Field s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecodeWithQuotes f = fieldDecodeWithSpaces (f . _value)

-- | Build a 'FieldDecode' from a function.
--
-- This version gives you access to the whole 'SpacedField', which includes
-- information about spacing both before and after the field, and about quotes
-- if they were used.
fieldDecodeWithSpaces :: (SpacedField s -> DecodeValidation e a) -> FieldDecode e s a
fieldDecodeWithSpaces f =
  FieldDecode . Compose . DecodeState . ReaderT $ \v -> state $ \(Ind i) ->
    if i >= length v
    then (unexpectedEndOfRow, Ind i)
    else (f (v ! i), Ind (i+1))

-- | promotes a FieldDecode to work on a whole 'Record' at once
promote :: (Textual e, Textual s) => FieldDecode e s a -> Record s -> DecodeValidation e a
promote dec rs =
  let vec = V.fromList . toList . _fields $ rs
      len = length vec
  in  case runFieldDecode dec vec (Ind 0) of
    (d, Ind i) ->
      if i >= len
      then d
      else d *> expectedEndOfRow (fmap (fmap (fmap retext)) (V.force (V.drop i vec)))
