module Data.Sv.Example.Concat where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Trifecta (TokenParsing, CharParsing, double, parseFromFile, char)
import System.Exit (exitFailure)

import Data.Sv.Syntax
import Data.Sv.Parse
import qualified Data.Sv.Decode as D

file :: FilePath
file = "csv/concat.csv"

opts :: ParseOptions ByteString
opts = defaultParseOptions & endOnBlankLine .~ True

type Name = Text
type Age = Int
data Person = Person Name Age deriving Show

person :: D.Decode' ByteString Person
person = Person <$> D.utf8 <*> D.int

type Stock = Int
newtype Cost = Cost Double deriving Show
data Item = Item Name Stock Cost deriving Show

cost :: TokenParsing m => m Cost
cost = char '$' *> fmap Cost double

item :: D.Decode' ByteString Item
item = Item <$> D.utf8 <*> D.int <*> D.withTrifecta cost

sv2 :: (CharParsing m) => ParseOptions s -> m (Sv s, Sv s)
sv2 o = (,) <$> separatedValues o <*> separatedValues o

data PeopleAndItems = PeopleAndItems [Person] [Item] deriving Show

parser :: CharParsing m => m (Sv ByteString, Sv ByteString)
parser = sv2 opts

main :: IO ()
main = do
  d <- parseFromFile parser file
  case d of
    Nothing -> exitFailure
    Just (s1,s2) -> do
      let result = PeopleAndItems <$> D.decode person s1 <*> D.decode item s2
      case result of
        D.Failure e -> do
          print e
          exitFailure
        D.Success a -> print a
