{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Sv.Print.Options (
  PrintOptions' (..)
  , PrintOptions
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

data PrintOptions' s t =
  PrintOptions {
    _build :: t -> Builder
  , _escape :: Escaper' s t
  }

class HasPrintOptions c s t | c -> s t where
  printOptions :: Lens' c (PrintOptions' s t)
  build :: Lens' c (t -> Builder)
  {-# INLINE build #-}
  escape :: Lens' c (Escaper' s t)
  {-# INLINE escape #-}
  build = printOptions . build
  escape = printOptions . escape

instance HasPrintOptions (PrintOptions' s t) s t where
  printOptions = id
  build f (PrintOptions x1 x2) = fmap (\ y -> PrintOptions y x2) (f x1)
  {-# INLINE build #-}
  escape f (PrintOptions x1 x2) = fmap (PrintOptions x1) (f x2)
  {-# INLINE escape #-}

type PrintOptions a = PrintOptions' a a

defaultPrintOptions :: PrintOptions BS.ByteString
defaultPrintOptions = utf8PrintOptions

utf8PrintOptions :: PrintOptions BS.ByteString
utf8PrintOptions = PrintOptions Builder.byteString escapeUtf8

utf8LazyPrintOptions :: PrintOptions BL.ByteString
utf8LazyPrintOptions = PrintOptions Builder.lazyByteString escapeUtf8Lazy

textPrintOptions :: PrintOptions Text
textPrintOptions = PrintOptions (Builder.byteString . Text.encodeUtf8) escapeText

stringPrintOptions :: PrintOptions String
stringPrintOptions = PrintOptions Builder.stringUtf8 escapeString