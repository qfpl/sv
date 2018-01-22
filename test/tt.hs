import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (unless)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Thyme
import System.Locale (defaultTimeLocale)
import System.Exit (exitFailure)
import Text.Parser.Char (char)
import Text.Parser.Token (integer)

import Data.Sv hiding (integer)


-- Table tennis handicaps

file :: FilePath
file = "test/tt-handicap.csv"

config :: SvConfig
config = defaultConfig & (headedness .~ Unheaded)

type Name = Text

data Difference =
  Plus Integer | Minus Integer
  deriving (Eq, Ord, Show)

data Handicap =
  Handicap YearMonthDay Difference Name Name
  deriving (Eq, Ord, Show)

ymd :: FieldDecode' Text YearMonthDay
ymd = attoparsec (
    buildTime <$> timeParser defaultTimeLocale "%Y%m%d"
  )

handicap :: FieldDecode' Text Handicap
handicap = Handicap <$> ymd <*> difference <*> text <*> text

difference :: FieldDecode' Text Difference
difference = attoparsec (
    (char '+' $> Plus <|> char '-' $> Minus) <*> integer
  )

main :: IO ()
main = do
  result <- decodeFromFile handicap (Just config) file
  case result of
    AccFailure e -> print e >> exitFailure
    AccSuccess h -> unless (length h == 5) exitFailure
