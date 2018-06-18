module Data.Sv.Cursor.Headedness where

import Control.Lens (Lens')

-- | Does the CSV have a 'Header' or not? A header is a row at the beginning
-- of a file which contains the string names of each of the columns.
--
-- If a header is present, it must not be decoded with the rest of the data.
data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

-- | Classy lens for 'Headedness'
class HasHeadedness c where
  headedness :: Lens' c Headedness

instance HasHeadedness Headedness where
  headedness = id
