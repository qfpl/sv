{-# LANGUAGE MultiParamTypeClasses #-}

module Data.ByteString1 (
  ByteString1 (ByteString1, toByteString)
, byteString1
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype ByteString1 =
  ByteString1 { toByteString :: ByteString }
  deriving (Eq, Ord)

byteString1 :: ByteString -> Maybe ByteString1
byteString1 bs = if not (BS.null bs) then Just (ByteString1 bs) else Nothing
