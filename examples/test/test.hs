import qualified Data.Sv.Example.Encoding as Encoding
import qualified Data.Sv.Example.Numbers as Numbers
import qualified Data.Sv.Example.Ragged as Ragged
import qualified Data.Sv.Example.Species as Species
import qualified Data.Sv.Example.TableTennis as TableTennis

main :: IO ()
main = do
  Encoding.main
  Numbers.main
  Ragged.main
  Species.main
  TableTennis.main
