module Data.Csv.Record
( module X
, nonEmptyRecord
) where

import Control.Lens (Prism')

import Data.Csv.Record.Final    as X
import Data.Csv.Record.NonEmpty as X
import Data.Csv.Record.Simple   as X
import Data.List.NonEmpty.Extra (AsNonEmpty, _NonEmpty)

-- Cannot live in NonEmptyRecord because of a module cycle
nonEmptyRecord :: AsNonEmpty s1 s2 => Prism' (Record s2) (NonEmptyRecord s1 s2)
nonEmptyRecord = _NonEmpty
