{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.TableTennis where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Time
--import System.Locale (defaultTimeLocale)
import System.Exit (exitFailure)
import Text.Parser.Char (char)
import Text.Parser.Token (integer)

import Data.Sv hiding (integer)

-- Table tennis handicaps

file :: FilePath
file = "csv/tt-handicap.csv"

config :: ParseOptions
config = defaultParseOptions & (headedness .~ Unheaded)

type Name = Text

data Difference =
  Plus Integer | Minus Integer
  deriving (Eq, Ord, Show)

data Handicap =
  Handicap Day Difference Name Name
  deriving (Eq, Ord, Show)

day :: FieldDecode' ByteString Day
day =
  string >>==
    validateMay (BadDecode "Invalid time") . parseTimeM True defaultTimeLocale "%Y%0m%0d"

handicap :: FieldDecode' ByteString Handicap
handicap = Handicap <$> day <*> difference <*> utf8 <*> utf8

difference :: FieldDecode' ByteString Difference
difference = attoparsec (
    (char '+' $> Plus <|> char '-' $> Minus) <*> integer
  )

main :: IO ()
main = do
  result <- decodeFromFile handicap (Just config) file
  case result of
    AccFailure e -> print e >> exitFailure
    AccSuccess h -> unless (length h == 5) exitFailure
