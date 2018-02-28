{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.Encoding where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Encode as E

expectedFile :: FilePath
expectedFile = "csv/encoding.expected.csv"

-- | Here's our data type to encode. It has a few standard types as well as
-- some other algebraic data types we're about to define.
data Example =
  Example ByteString Text Int Double Product Sum
  deriving Show

data Product =
  Product Text Double
  deriving Show

data Sum
  = Sum1 Int
  | Sum2 Double
  | Sum3 Text
  deriving Show

-- | Here we're defining an encoder for a 'Product' by using the
-- 'divide' combinator.
--
-- 'divide' takes a function to split up a product into a tuple, and an encoder
-- for each side of the tuple, and returns you an 'Encode' for the whole thing.
--
-- So in this case we split the Product into a tuple of its two components,
-- namely a 'Text' and a 'Double', and then provide the primitive encoders for
-- those two.
--
-- @
-- divide :: (a -> (b,c)) -> Encode b -> Encode c -> Encode a
-- @
--
-- or in our case
--
-- @
-- divide :: (Produt -> (Text, Int)) -> Encode Text -> Encode Int -> Encode Product
-- @
productEnc :: Encode Product
productEnc = E.divide (\(Product t d) -> (t,d)) E.text E.double

-- | Here we're defining an encoder for a 'Sum' using the 'choose' combinator.
--
-- 'choose' takes a function to split a sum into an 'Either' and an encoder
-- for both the left and right possibilities, and returns an encoder for the
-- whole structure.
--
-- Since 'Sum' has three cases, we're doing all the splitting in 'choose' by
-- layering recursive 'Either's, and then using the 'chosen' combinator to take
-- care of the right hand side.
--
-- @
-- 'choose' :: (a -> Either b c) -> Encode b -> Encode c -> Encode a
-- 'chosen' :: Encode b -> Encode c -> Encode (Either b c)
-- @
--
-- Here are some more types to help understand how this example fits together
--
-- @
-- split :: Sum -> Either Int (Either Double Text)
-- E.choose split :: Encode Int -> Encode (Either Double Text) -> Encode Sum
-- E.chosen E.double E.text :: Encode (Either Double Text)
-- @

sumEnc :: Encode Sum
sumEnc = E.choose split E.int $ E.chosen E.double E.text
  where
    split s =
      case s of
        Sum1 i -> Left i
        Sum2 d -> Right (Left d)
        Sum3 t -> Right (Right t)

exampleEnc :: Encode Example
exampleEnc =
  E.divide (\(Example b t i d p s) -> (b,(t,(i,(d,(p,s)))))) E.byteString $
    E.divided E.text $
    E.divided E.int $
    E.divided E.double $
    E.divided productEnc sumEnc

examples :: [Example]
examples =
  [ Example "Hello" "Goodbye" 5 5.1 (Product "text" 0.5) (Sum1 20)
  , Example "Yes" "no" 200 (-4.5) (Product "" 22) (Sum2 19.3)
  , Example "a" "b" 0 0 (Product "words words words" 15) (Sum3 "More words")
  ]

opts :: EncodeOptions
opts = defaultEncodeOptions

main :: IO ()
main = do
  expected <- LBS.readFile expectedFile
  putStrLn "Data to encode:"
  print examples
  putStrLn "Expected"
  LBS.putStr expected
  putStrLn ""
  putStrLn "Encoded:"
  let encoded = encode opts exampleEnc examples
  LBS.putStr encoded
  putStrLn ""
  when (expected /= encoded) exitFailure
