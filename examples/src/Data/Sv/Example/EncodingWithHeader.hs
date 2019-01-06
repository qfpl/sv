{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.Example.EncodingWithHeader (main) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as LBS
import Data.Semigroup ((<>))
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Encode as E
import Data.Sv.Example.Encoding (Example, sumEnc, examples, e1,e2,e3,e4,e5,e6,p1,p2)

-- This module demonstrates encoding with a header.
--
-- It is worth looking at Encoding.hs before this module, as this module
-- imports definitions from that module.
--
-- EncodingWithHeader.hs focuses on encoding with a header, whereas Encode.hs
-- is mostly about showing off different ways to define an encoder.

expectedFile :: FilePath
expectedFile = "csv/encoding-header.expected.csv"

opts :: EncodeOptions
opts = defaultEncodeOptions

-- Here we build our encoder, attaching a name to each column
exampleEnc :: NameEncode Example
exampleEnc =
      "Bytestring" =: E.encodeOf e1 E.byteString
  <>  "Text" =: E.encodeOf e2 E.text
  <>  "Int" =: E.encodeOf e3 E.int
  <>  "Double" =: E.encodeOf e4 E.doubleFast
  -- Notice that we have had to inline the definition of productEnc to
  -- attach a name to each component of it.
  --
  -- The lesson is that if one wants encoding with column names, this should
  -- be built-in as you go. It can be tricky to attach names to the right
  -- places after the fact.
  -- This could also be avoided by not using ADTs within the data
  -- type to be encoded.
  <>  "Text2" =: E.encodeOf (e5.p1) E.text
  <>  "Double2" =: E.encodeOf (e5.p2) E.doubleFast
  <>  "Sum" =: E.encodeOf e6 sumEnc



main :: IO ()
main = do
  expected <- LBS.readFile expectedFile
  putStrLn "Data to encode:"
  print examples
  putStrLn "Expected"
  LBS.putStr expected
  putStrLn ""
  putStrLn "Encoded:"
  let encoded = encodeNamed exampleEnc opts examples
  LBS.putStr encoded
  putStrLn ""

  -- validation
  when (expected /= encoded)
    (putStrLn "expected /= encoded" *> exitFailure)
