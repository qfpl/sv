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
import Control.Monad.State (state)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Compose.Extra (rmapC)
import Data.Validation (_AccValidation, bindValidation)

import Data.Sv.Field (Field, SpacedField, fieldContents)
import Data.Sv.Decode.Error
import Data.Sv.Decode.State (runDecodeState)
import Data.Sv.Decode.Type
import Data.Sv.Record (Record, _fields)
import Text.Babel (Textual (retext))
import Text.Space (Spaced (_value))

-- | Convenience to get the underlying function out of a FieldDecode in a useful form
runFieldDecode :: FieldDecode e s a -> [SpacedField s] -> (DecodeValidation e a, [SpacedField s])
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
fieldDecodeWithSpaces f = FieldDecode . Compose . state $ \l ->
  case l of
    [] -> (unexpectedEndOfRow, [])
    (x:xs) -> (f x, xs)

-- | promotes a FieldDecode to work on a whole 'Record' at once
promote :: (Textual e, Textual s) => FieldDecode e s a -> Record s -> DecodeValidation e a
promote a rs =
  case runFieldDecode a . toList . _fields $ rs of
    (v, []) -> v
    (v, xs@(_:_)) -> v *> expectedEndOfRow (fmap (fmap (fmap retext)) xs)
