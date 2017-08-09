import Test.Tasty

import Data.CSV.ParserTest (test_Parser)
import Data.CSV.PrettyTest (test_Pretty)


main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parser
  , test_Pretty
  ]

