-- | Chunks of text separated by escape sequences
module Text.Escaped (
  Escaped
  , Escape
  , noEscape
  , escaped
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup     (Semigroup ((<>)))
import Data.Separated     (Pesarated1 (Pesarated1), Separated1 (Separated1), sprinkle, singlePesarated)

-- | A trivial type to be our separator. The actual value of the escape is
-- determined externally.
data Escape =
  Escape
  deriving (Eq, Ord, Show)

instance Semigroup Escape where
  Escape <> Escape = Escape

instance Monoid Escape where
  mappend = (<>)
  mempty = Escape

-- | Escaped is for representing text containing zero or more occurances of an
-- escape sequence.
--
-- This representation only works when there is one escape
-- sequence, as it does not contain any information about which escape sequence
-- occured. The @Pesarated1@ type could be used directly for that purpose.
type Escaped a =
  Pesarated1 Escape a

-- | noEscape is a dramatically-named singleton constructor for Escaped.
noEscape :: a -> Escaped a
noEscape = singlePesarated

-- | Put escape sequences in between the elements of a non-empty list.
escaped :: NonEmpty a -> Escaped a
escaped (a:|as) =
  Pesarated1 (Separated1 a (sprinkle Escape as))

