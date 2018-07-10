{-|
Module      : Data.Sv.Cursor.Newline
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Cursor.Newline (
  Newline (UnsafeMkNewline, toByteString)
, newlineToBuilder
, crlf
, lf
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.UTF8 as UTF8

-- | 'Newline' is a newtype around 'ByteString'
newtype Newline
  = UnsafeMkNewline { toByteString :: ByteString }
    deriving (Eq, Ord)

instance Show Newline where
  showsPrec _ = showString . UTF8.toString . toByteString

-- | Convert a 'Newline' to a ByteString 'Builder'. This is used by
-- encoding.
newlineToBuilder :: Newline -> Builder
newlineToBuilder = byteString . toByteString

-- | Unix/Linux newlines: a line feed character
lf :: Newline
lf = UnsafeMkNewline "\n"

-- | DOS newlines: a carriage return character followed by a
-- line feed character
crlf :: Newline
crlf = UnsafeMkNewline "\r\n"

