{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.Numbers where

import Control.Lens ((&), (.~))
import Data.Functor (($>))
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D

-- | This example demonstrates some of the combinators for handling
-- different values representing missing or not applicable data.

file :: FilePath
file = "csv/numbers.csv"

opts :: ParseOptions ByteString
opts = defaultParseOptions & headedness .~ Unheaded

num :: FieldDecode' ByteString (Maybe Double)
num = D.orEmpty D.double
  <!> (D.exactly "unknown" <!> D.exactly "NULL") $> Nothing

main :: IO ()
main = do
  v <- parseDecodeFromFile num opts file
  case v of
    Failure e -> do
      putStrLn ("Failed to parse and decode " <> file)
      print e
      exitFailure
    Success a ->
      print a
