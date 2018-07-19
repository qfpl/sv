{-|
Module      : Data.Sv.Structure.Core
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Structure.Core (
  Separator
, comma
, pipe
, tab
, HasSeparator (separator)
, Headedness (..)
, HasHeadedness (..)
) where

import Data.Sv.Structure.Headedness
import Data.Sv.Structure.Separator
