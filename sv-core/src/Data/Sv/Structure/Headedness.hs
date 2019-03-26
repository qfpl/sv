{-|
Module      : Data.Sv.Structure.Headedness
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Structure.Headedness (
  Headedness (..)
, HasHeadedness (..)
) where

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
