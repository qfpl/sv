{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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

Newtypes to keep track of which text is in an escaped form and which is not
-}

module Text.Escape (
  Unescaped (Unescaped, getUnescaped)
  , HasUnescaped (unescaped, unescapedValue)
  , Escaped (UnsafeEscaped)
  , getRawEscaped
  , escapeString
  , escapeText
  , escapeUtf8
  , escapeLazyUtf8
  , escapeChar
  , Escapable (escape, escape_)
) where

import Control.DeepSeq    (NFData)
import Control.Lens       (Lens', iso, Wrapped(_Wrapped'), Unwrapped, Rewrapped)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import Data.Foldable      (Foldable)
import Data.Functor       (Functor)
import Data.Monoid        (Monoid)
import Data.Semigroup     (Semigroup ((<>)))
import Data.Text          (Text)
import qualified Data.Text as Text
import Data.Traversable   (Traversable)
import GHC.Generics       (Generic)

-- | Wrapper for text that is known to be in an unescaped form
newtype Unescaped a =
  Unescaped { getUnescaped :: a }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Unescaped a)

instance Unescaped a1 ~ t => Rewrapped (Unescaped a2) t

instance Wrapped (Unescaped a) where
  type Unwrapped (Unescaped a) = a
  _Wrapped' = iso (\ (Unescaped a) -> a) Unescaped

-- | Classy lenses for 'Unescaped'
class HasUnescaped c a | c -> a where
  unescaped :: Lens' c (Unescaped a)
  unescapedValue :: Lens' c a
  {-# INLINE unescapedValue #-}
  unescapedValue = unescaped . unescapedValue

instance HasUnescaped (Unescaped a) a where
  unescaped = id
  {-# INLINE unescaped #-}
  unescapedValue = _Wrapped'
  {-# INLINE unescapedValue #-}

-- | Wrapper for text that is known to be in an escaped form.
--
-- The constructor should not be called directly unless you know the
-- appropriate escape, and have performed it yourself.
newtype Escaped a =
  UnsafeEscaped a
  deriving (Eq, Ord, Show, Semigroup, Monoid, Foldable)

-- | Unwrap an 'Escaped' value. This is intentionally not a record selector.
getRawEscaped :: Escaped a -> a
getRawEscaped (UnsafeEscaped a) = a

-- | Replaces all occurrences of the given character with two occurrences of that
-- character, non-recursively, in the given 'String'.
--
-- >>> escapeString ''' "hello 'string'"
-- "hello ''string''"
--
escapeString :: Char -> String -> Escaped String
escapeString c s = UnsafeEscaped (concatMap (doubleChar c) s)

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given 'Text'
--
-- Assuming @{- LANGUAGE OverloadedStrings -}@:
--
-- >>> escapeText ''' "hello 'text'"
-- "hello ''text''"
--
escapeText :: Char -> Text -> Escaped Text
escapeText c s =
  let ct = Text.singleton c
  in  UnsafeEscaped (Text.replace ct (ct <> ct) s)

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given ByteString, which is assumed to be UTF-8 compatible.
--
-- Assuming @{- LANGUAGE OverloadedStrings -}@:
--
-- >>> escapeUtf8 ''' "hello 'bytestring'"
-- "hello ''bytestring''"
--
escapeUtf8 :: Char -> B.ByteString -> Escaped B.ByteString
escapeUtf8 c =
  UnsafeEscaped . UTF8.fromString . concatMap (doubleChar c) . UTF8.toString

-- | Replaces all occurrences of the given character with two occurrences of that
-- character in the given lazy ByteString, which is assumed to be UTF-8 compatible.
--
-- Assuming @{- LANGUAGE OverloadedStrings -}@:
--
-- >>> escapeLazyUtf8 ''' "hello 'lazy bytestring'"
-- "hello ''lazy bytestring''"
--
escapeLazyUtf8 :: Char -> L.ByteString -> Escaped L.ByteString
escapeLazyUtf8 c =
  UnsafeEscaped . UTF8L.fromString . concatMap (doubleChar c) . UTF8L.toString

-- | Escape a character, which must return a string.
--
-- >>> escapeChar ''' '''
-- "''"
--
-- >>> escapeChar ''' 'z'
-- "z"
--
escapeChar :: Char -> Char -> Escaped String
escapeChar c b =
  UnsafeEscaped $ if c == b then [b,b] else [b]

doubleChar :: Char -> Char -> String
doubleChar q z = if z == q then [q,q] else [z]

-- | This class is for text in which a particular character can be escaped to
-- produce an 'Escaped'
class Escapable a where
  escape :: Char -> a -> Escaped a
  escape_ :: Char -> Unescaped a -> Escaped a
  escape_ c = escape c . getUnescaped

instance Escapable String where
  escape = escapeString

instance Escapable Text where
  escape = escapeText

instance Escapable B.ByteString where
  escape = escapeUtf8

instance Escapable L.ByteString where
  escape = escapeLazyUtf8
