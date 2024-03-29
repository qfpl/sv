{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.Example.Encoding where

import Contravariant.Extras.Contrazip (contrazip6)
import Control.Lens (makeLenses, makePrisms)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Encode as E

expectedFile :: FilePath
expectedFile = "csv/encoding.expected.csv"

data Product =
  Product {
    _p1 :: Text
  , _p2 :: Double
  }
  deriving Show

makeLenses ''Product

data Sum
  = Sum1 Int
  | Sum2 Double
  | Sum3 Text
  deriving Show

makePrisms ''Sum

-- | Here's our data type to encode. It has a few standard types as well as
-- some other algebraic data types we're about to define.
data Example =
  Example {
    _e1 :: ByteString
  , _e2 :: Text
  , _e3 :: Int
  , _e4 :: Double
  , _e5 :: Product
  , _e6 :: Sum
  }
  deriving Show

makeLenses ''Example

-- | Here we're defining an encoder for a 'Product' by using the
-- 'contramap' and '<>' combinators.
productEnc :: Encode Product
productEnc = contramap _p1 E.text
             <> contramap _p2 E.doubleFast

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
-- choose split :: Encode Int -> Encode (Either Double Text) -> Encode Sum
-- chosen E.doubleFast E.text :: Encode (Either Double Text)
-- @
--
sumEnc :: Encode Sum
sumEnc = choose split E.int $ chosen E.doubleFast E.text
  where
    split s =
      case s of
        Sum1 i -> Left i
        Sum2 d -> Right (Left d)
        Sum3 t -> Right (Right t)

-- | 'exampleEnc' puts it all together. 'Example' is a product, so we
-- use 'contramap' and '<>'.
exampleEnc :: Encode Example
exampleEnc =
  contramap _e1 E.byteString
  <> contramap _e2 E.text
  <> contramap _e3 E.int
  <> contramap _e4 E.doubleFast
  <> contramap _e5 productEnc
  <> contramap _e6 sumEnc

examples :: [Example]
examples =
  [ Example "Hello" "Goodbye" 5 5.1 (Product "text" 0.5) (Sum1 20)
  , Example "Yes" "no" 200 (-4.5) (Product "" 22) (Sum2 19.3)
  , Example "a" "b" 0 0 (Product "words \"words\" words" 15) (Sum3 "More words")
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
  let encoded = encode exampleEnc opts examples
  LBS.putStr encoded
  putStrLn ""

  -- validation
  when (expected /= encoded) $ putStrLn "expected /= encoded" *> exitFailure
  let encoded2 = encode exampleEncContravariantExtras opts examples
  let encoded3 = encode exampleEncLens opts examples
  when (expected /= encoded2) $ putStrLn "expected /= encoded2" *> exitFailure
  when (expected /= encoded3) $ putStrLn "expected /= encoded3" *> exitFailure

-- | Bonus Round #1
--
-- Here is an alternative definition of exampleEnc that you might prefer.
-- This one depends on the @contravariant-extras@ package from hackage,
-- which provides 'contrazip6' along with many other @contrazipN@ functions.
exampleEncContravariantExtras :: Encode Example
exampleEncContravariantExtras =
  contramap (\(Example b t i d p s) -> (b,t,i,d,p,s)) $
    contrazip6 E.byteString E.text E.int E.doubleFast productEnc sumEnc

-- | Bonus Round #2
--
-- In this alternative, we're using lenses and prisms (derived automatically
-- via 'makeLenses' and 'makePrisms') with the 'encodeOf' combinator and the
-- 'Semigroup' instance on 'Encode'.
--
-- 'encodeOf' works with optics of many types (Lens, Prism, Traversal, Getter)
-- and builds an encoder based on running that optic.
--
-- If you did not want to use lens, you could use 'contramap' with
-- field accessor functions.
--
-- This version is pretty clean. It's my favourite of the three :)
productEncLens :: Encode Product
productEncLens =
  E.encodeOf p1 E.text <> E.encodeOf p2 E.doubleFast

sumEncLens :: Encode Sum
sumEncLens =
    E.encodeOf _Sum1 E.int <> E.encodeOf _Sum2 E.doubleFast <> E.encodeOf _Sum3 E.text

exampleEncLens :: Encode Example
exampleEncLens =
      E.encodeOf e1 E.byteString
  <>  E.encodeOf e2 E.text
  <>  E.encodeOf e3 E.int
  <>  E.encodeOf e4 E.doubleFast
  <>  E.encodeOf e5 productEncLens
  <>  E.encodeOf e6 sumEncLens
