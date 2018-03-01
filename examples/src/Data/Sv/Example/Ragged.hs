module Data.Sv.Example.Ragged where

import Data.Functor.Alt
import Data.ByteString (ByteString)
import Data.Text (Text)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D

file :: FilePath
file = "csv/ragged.csv"

type Name = Text
type Age = Int

data Person =
  OneName Name Age
  | TwoNames Name Name Age
  deriving (Eq,Ord,Show)

personDecoder :: Decode ByteString ByteString Person
personDecoder =
  OneName <$> D.utf8 <*> D.int
  <!>
  TwoNames <$> D.utf8 <*> D.utf8 <*> D.int

main :: IO ()
main = do
  v <- parseDecodeFromFile personDecoder defaultParseOptions file
  case v of
    Failure e -> do
      putStrLn "Failed to parse and decode ragged.csv:"
      print e
      exitFailure
    Success a ->
      print a
