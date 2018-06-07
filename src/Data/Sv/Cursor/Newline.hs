{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Sv.Cursor.Newline
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

|-}

module Data.Sv.Cursor.Newline (
  Newline (UnsafeMkNewline, toByteString)
, newlineToBuilder
, crlf
, lf
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.UTF8 as UTF8

newtype Newline
  = UnsafeMkNewline { toByteString :: ByteString }
    deriving (Eq, Ord)

instance Show Newline where
  showsPrec _ = showString . UTF8.toString . toByteString

newlineToBuilder :: Newline -> Builder
newlineToBuilder = byteString . toByteString

crlf :: Newline
crlf = UnsafeMkNewline "\r\n"

lf :: Newline
lf = UnsafeMkNewline "\n"
