{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Text.Escape
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Quote characters can be escaped in CSV documents by using two quote characters
instead of one. sv's parser will unescape these sequences as it parses them, so
it wraps them in the newtype 'Unescaped'

Encoding requires you to provide an 'Escaper', which is a function to escape
strings on the way out.
-}

module Text.Escape (
  Unescaped (Unescaped, getRawUnescaped)
  , Escaper
  , Escaper'
  , escapeString
  , escapeText
  , escapeUtf8
  , escapeUtf8Lazy
  , escapeChar
) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import Data.Foldable (Foldable)
import Data.Functor (Functor)
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

-- | Wrapper for text that is known to be in an unescaped form
newtype Unescaped a =
  Unescaped { getRawUnescaped :: a }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Unescaped a)

-- | A function that, given a char, escapes all occurrences of that char.
--
-- This version allows the escaping to be type-changing. For example, escaping
-- a single char can result in a string with two characters.
type Escaper s t = Char -> Unescaped s -> t

-- | A function that, given a char, escapes all occurrences of that char.
type Escaper' a = Char -> Unescaped a -> a

-- | Replaces all occurrences of the given character with two occurrences of that
-- character, non-recursively, in the given 'String'.
--
-- >>> escapeString ''' "hello 'string'"
-- "hello ''string''"
--
escapeString :: Escaper' String
escapeString c = concatMap (doubleChar c) . getRawUnescaped

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given 'Text'
--
-- @
-- {- LANGUAGE OverloadedStrings -}
--
-- >>> escapeText ''' "hello 'text'"
-- "hello ''text''"
-- @
escapeText :: Escaper' Text
escapeText c =
  let ct = Text.singleton c
  in  Text.replace ct (ct <> ct) . getRawUnescaped

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given ByteString, which is assumed to be UTF-8 compatible.
--
-- @
-- {- LANGUAGE OverloadedStrings -}
-- >>> escapeUtf8 ''' "hello 'bytestring'"
-- "hello ''bytestring''"
-- @
escapeUtf8 :: Escaper' B.ByteString
escapeUtf8 c =
  UTF8.fromString . concatMap (doubleChar c) . UTF8.toString . getRawUnescaped

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given lazy ByteString, which is assumed to be UTF-8 compatible.
--
-- @
-- {- LANGUAGE OverloadedStrings -}
--
-- >>> escapeUtf8Lazy ''' "hello 'lazy bytestring'"
-- "hello ''lazy bytestring''"
-- @
escapeUtf8Lazy :: Escaper' L.ByteString
escapeUtf8Lazy c =
  UTF8L.fromString . concatMap (doubleChar c) . UTF8L.toString . getRawUnescaped

-- | Escape a character, which must return a string.
--
-- >>> escapeChar ''' '''
-- "''"
--
-- >>> escapeChar ''' 'z'
-- "z"
--
escapeChar :: Escaper Char String
escapeChar c = doubleChar c . getRawUnescaped

doubleChar :: Char -> Char -> String
doubleChar q z = if z == q then [q,q] else [z]
