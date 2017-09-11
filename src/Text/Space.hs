-- | Large chunks of only space characters, represented efficiently as integers
module Text.Space where

import Data.Monoid      (Monoid (mappend, mempty))
import Data.Semigroup   (Semigroup ((<>)))
import Data.String      (IsString (fromString))

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

-- | Turn @Spaces@ into a string
-- TODO replace this with a prism
spaces :: IsString s => Spaces -> s
spaces s = fromString (replicate (countSpaces s) ' ')

-- | An `a` between spaces is @Spaced@
type Spaced a = Between Spaces a 

-- | Alias for @Text.Between.betwixt@
spaced :: Spaces -> Spaces -> a -> Spaced a
spaced = betwixt

