{-# LANGUAGE OverloadedStrings #-}

-- | Large chunks of only space characters, represented efficiently as integers
module Text.Space where

import Control.Lens     (Prism', prism')
import Data.Monoid      (Monoid (mappend, mempty))
import Data.Semigroup   (Semigroup ((<>)))
import Data.Text        (Text)
import qualified Data.Text as Text (replicate, unpack)

import Text.Between     (Between, betwixt)

-- | Large chunks of only space characters, represented efficiently as integers.
-- A convenient monoid instance is included.
newtype Spaces =
  Spaces { countSpaces :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup Spaces where
  Spaces x <> Spaces y = Spaces (x + y)

-- | The addition monoid for integers is chosen for `Spaces`
-- so that conversion to string types is a monoid homomorphism
instance Monoid Spaces where
  mempty = Spaces 0
  mappend = (<>)

-- | One space
single :: Spaces
single = Spaces 1

-- | Parse 'Text' into 'Spaces', or turn spaces into text
spaces :: Prism' Text Spaces
spaces =
  prism'
    (\s -> (Text.replicate (countSpaces s) " "))
    (foldMap c2s . Text.unpack)

c2s :: Char -> Maybe Spaces
c2s ' ' = Just single
c2s _   = Nothing

-- | Something between spaces is 'Spaced'
type Spaced = Between Spaces

-- | Alias for @Text.Between.betwixt@
spaced :: Spaces -> Spaces -> a -> Spaced a
spaced = betwixt

