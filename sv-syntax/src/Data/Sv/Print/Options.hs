{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Sv.Print.Options
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Print.Options (
  PrintOptions (..)
  , HasPrintOptions (..)
  , defaultPrintOptions
  , utf8PrintOptions
  , utf8LazyPrintOptions
  , textPrintOptions
  , stringPrintOptions
) where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Text.Escape (Escaper', escapeUtf8, escapeUtf8Lazy, escapeText, escapeString)

-- | Options to configure the printing process
data PrintOptions a =
  PrintOptions {
    -- | How do I convert these values into ByteString 'Builder's? This depends
    -- not only on type, but on character encoding. Default: 'utf8PrintOptions'
    _build :: a -> Builder
    -- | How do I escape quotes which appear in this value? Default: 'escapeUtf8'
  , _escape :: Escaper' a
  }

-- | Classy optics for 'PrintOptions'
class HasPrintOptions c a | c -> a where
  printOptions :: Lens' c (PrintOptions a)
  build :: Lens' c (a -> Builder)
  {-# INLINE build #-}
  escape :: Lens' c (Escaper' a)
  {-# INLINE escape #-}
  build = printOptions . build
  escape = printOptions . escape

instance HasPrintOptions (PrintOptions a) a where
  printOptions = id
  {-# INLINE printOptions #-}
  build f (PrintOptions x1 x2) = fmap (\ y -> PrintOptions y x2) (f x1)
  {-# INLINE build #-}
  escape f (PrintOptions x1 x2) = fmap (PrintOptions x1) (f x2)
  {-# INLINE escape #-}

-- | Print options for 'Sv's containing UTF-8 bytestrings
defaultPrintOptions :: PrintOptions BS.ByteString
defaultPrintOptions = utf8PrintOptions

-- | Print options for 'Sv's containing UTF-8 bytestrings
utf8PrintOptions :: PrintOptions BS.ByteString
utf8PrintOptions = PrintOptions Builder.byteString escapeUtf8

-- | Print options for 'Sv's containing UTF-8 lazy bytestrings
utf8LazyPrintOptions :: PrintOptions BL.ByteString
utf8LazyPrintOptions = PrintOptions Builder.lazyByteString escapeUtf8Lazy

-- | Print options for 'Sv's containing 'Text'
textPrintOptions :: PrintOptions Text
textPrintOptions = PrintOptions (Builder.byteString . Text.encodeUtf8) escapeText

-- | Print options for 'Sv's containing 'String's
stringPrintOptions :: PrintOptions String
stringPrintOptions = PrintOptions Builder.stringUtf8 escapeString