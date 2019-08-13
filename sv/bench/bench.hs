{-# language OverloadedStrings #-}

module Main (main) where

import Control.Lens ((&), (.~))
import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import HaskellWorks.Data.Dsv.Lazy.Cursor (DsvCursor, makeCursor)

import Data.Sv
import qualified Data.Sv.Decode as D

opts :: ParseOptions
opts = defaultParseOptions & headedness .~ Unheaded

pd :: Decode' BS8.ByteString a -> DsvCursor -> DecodeValidation BS8.ByteString [a]
pd dec = parseDecodeFromDsvCursor dec opts

main :: IO ()
main =
  defaultMain
      [ bench "double" $ nf (pd D.double) doublesC
      , bench "read double" $ nf (pd (D.read :: Decode' BS.ByteString Double)) doublesC
      , bench "float" $ nf (pd D.float) doublesC
      , bench "read float" $ nf (pd (D.read :: Decode' BS.ByteString Float)) doublesC
      , bench "many" $ nf (parseDecode (D.utf8 <* many D.utf8) opts) "a\na"
      ]

doubles :: LBS.ByteString
doubles =
  LBS.fromStrict . BS.intercalate (BS8.singleton '\n') $
    fmap (BS8.pack . show) [0 :: Double, 1, 1.1102230246251565e-16, 7.845860130857695, 5000000000]

doublesC :: DsvCursor
doublesC = makeCursor 10 doubles
