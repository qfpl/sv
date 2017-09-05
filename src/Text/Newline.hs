module Text.Newline (
  Newline (CR, LF, CRLF)
  , newlineString
) where

import Data.String (IsString (fromString))

data Newline =
  CR | LF | CRLF
  deriving (Eq, Ord, Show)

newlineString :: IsString s => Newline -> s
newlineString n =
  fromString $ case n of
    CR -> "\r"
    LF -> "\n"
    CRLF -> "\r\n"

