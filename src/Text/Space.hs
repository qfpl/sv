module Text.Space where

import Data.Monoid      (Monoid (mappend, mempty))
import Data.Semigroup   (Semigroup ((<>)))
import Data.String      (IsString (fromString))

import Text.Between     (Between, betwixt)

newtype Spaces =
  Spaces { countSpaces :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup Spaces where
  Spaces x <> Spaces y = Spaces (x + y)

instance Monoid Spaces where
  mempty = Spaces 0
  mappend = (<>)

spaceChar :: Char
spaceChar = ' '

single :: Spaces
single = Spaces 1

spaces :: IsString s => Spaces -> s
spaces s = fromString (replicate (countSpaces s) spaceChar)

type Spaced a = Between Spaces a 

spaced :: Spaces -> Spaces -> a -> Spaced a
spaced = betwixt

