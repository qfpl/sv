{-
Copyright (c)2012, Johan Tibell

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Johan Tibell nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Data.Sv.Alien.Cassava where

import Data.ByteString.Builder (byteString, toLazyByteString, charUtf8)
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Unsafe as S
import GHC.Word (Word8)

type Field = S.ByteString

doubleQuote, newline, cr :: Word8
doubleQuote = 34
newline = 10
cr = 13

-- | A strict version of 'Data.Functor.<$>' for monads.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
    a <- m
    return $! f a
{-# INLINE (<$!>) #-}

infixl 4 <$!>

-- | Parse a field. The field may be in either the escaped or
-- non-escaped format. The return value is unescaped.
field :: AL.Parser Field
field = do
    _ <- A.skipWhile (== 32) -- space
    mb <- A.peekWord8
    -- We purposely don't use <|> as we want to commit to the first
    -- choice if we see a double quote.
    case mb of
        Just b | b == doubleQuote -> escapedField
        _                         -> unescapedField
{-# INLINE field #-}

escapedField :: AL.Parser S.ByteString
escapedField = do
    _ <- dquote
    -- The scan state is 'True' if the previous character was a double
    -- quote.  We need to drop a trailing double quote left by scan.
    s <- S.init <$> A.scan False (\s c -> if c == doubleQuote
                                          then Just (not s)
                                          else if s then Nothing
                                               else Just False)
    if doubleQuote `S.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
        else return s

unescapedField :: AL.Parser S.ByteString
unescapedField = A.takeWhile (\ c -> c /= doubleQuote &&
                                            c /= newline &&
                                            c /= cr)

dquote :: AL.Parser Char
dquote = char '"'

unescape :: Z.Parser S.ByteString
unescape = (toStrict . toLazyByteString) <$!> go mempty where
  go acc = do
    h <- Z.takeWhile (/= doubleQuote)
    let rest = do
          start <- Z.take 2
          if (S.unsafeHead start == doubleQuote &&
              S.unsafeIndex start 1 == doubleQuote)
              then go (acc `mappend` byteString h `mappend` charUtf8 '"')
              else fail "invalid CSV escape sequence"
    done <- Z.atEnd
    if done
      then return (acc `mappend` byteString h)
      else rest