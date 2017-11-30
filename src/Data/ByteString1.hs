{-# LANGUAGE MultiParamTypeClasses #-}

module Data.ByteString1 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype ByteString1 =
  ByteString1 { toByteString :: ByteString }
  deriving (Eq, Ord)

fromByteString :: ByteString -> Maybe ByteString1
fromByteString bs = if not (BS.null bs) then Just (ByteString1 bs) else Nothing
