module Data.Svfactor.Example.Concat where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Text.Trifecta (CharParsing, parseFromFile)
import System.Exit (exitFailure)

import Data.Svfactor.Syntax
import Data.Svfactor.Parse

file :: FilePath
file = "csv/concat.csv"

opts :: ParseOptions ByteString
opts = defaultParseOptions & endOnBlankLine .~ True

sv2 :: (CharParsing m) => ParseOptions s -> m (Sv s, Sv s)
sv2 o = (,) <$> separatedValues o <*> separatedValues o

parser :: CharParsing m => m (Sv ByteString, Sv ByteString)
parser = sv2 opts

main :: IO ()
main = do
  d <- parseFromFile parser file
  case d of
    Nothing -> exitFailure
    Just _  -> pure ()
