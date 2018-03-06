{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.TableTennis where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Time
import System.Exit (exitFailure)
import Text.Parser.Char (char)
import Text.Parser.Token (integer)

import Data.Sv
import qualified Data.Sv.Decode as D

-- Table tennis handicaps

file :: FilePath
file = "csv/tt-handicap.csv"

opts :: ParseOptions ByteString
opts = defaultParseOptions & headedness .~ Unheaded

type Name = Text

data Difference =
  Plus Integer | Minus Integer
  deriving (Eq, Ord, Show)

data Handicap =
  Handicap Day Difference Name Name
  deriving (Eq, Ord, Show)

day :: Decode' ByteString Day
day =
  D.string >>==
    validateMaybe (BadDecode "Invalid time") . parseTimeM True defaultTimeLocale "%Y%0m%0d"

handicap :: Decode' ByteString Handicap
handicap = Handicap <$> day <*> difference <*> D.utf8 <*> D.utf8

difference :: Decode' ByteString Difference
difference = D.withAttoparsec (
    (char '+' $> Plus <|> char '-' $> Minus) <*> integer
  )

main :: IO ()
main = do
  result <- parseDecodeFromFile handicap opts file
  case result of
    Failure e -> print e >> exitFailure
    Success h -> unless (length h == 5) exitFailure
