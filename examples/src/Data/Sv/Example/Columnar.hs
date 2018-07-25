{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.Columnar where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Sv
import Data.Sv.Decode as D
import System.Exit (exitFailure)

-- This is an example of using sv's column-name-based decoders
-- which decode based on column name rather than position.
--
-- Decoding this way makes our decoders tolerant to field reordering
-- and makes it much easier to extract only a subset of fields from
-- a large CSV.
-- To demonstrate this, this file decodes a CSV with far more columns than
-- this, and Common_name and Scientific_name are out of order.

type ID = Int

data Species =
  Species {
    taxonId :: ID
  , commonName :: ByteString
  , scientificName :: ByteString
  }
  deriving (Eq, Ord, Show)

speciesD :: NameDecode' ByteString Species
speciesD =
  Species <$> "Taxon_Id" .: D.int
          <*> "Common_name" .: D.contents
          <*> "Scientific_name" .: D.contents

file :: FilePath
file = "csv/species.csv"

-- Just for a bit of fun, let's find the species with the longest common name

newtype LongestName = LongestName { getSpecies :: Species }

instance Semigroup LongestName where
  s@(LongestName ss) <> t@(LongestName tt) =
    if BS.length (commonName ss) >= BS.length (commonName tt)
    then s else t

longestName :: NonEmpty Species -> Species
longestName = getSpecies . foldMap1 LongestName

main :: IO ()
main = do
  validation <- parseDecodeNamedFromFile speciesD defaultParseOptions file
  case validation of
    Failure e -> do print e
                    exitFailure
    Success a -> print (longestName <$> nonEmpty a)
