module Main (main) where

import Control.Lens ((&), (.~))
import Criterion.Main

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E

opts :: ParseOptions
opts = defaultParseOptions & headedness .~ Unheaded

main :: IO ()
main =
  defaultMain
      [
      ]
