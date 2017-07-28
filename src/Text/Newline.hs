module Text.Newline where

data Newline =
  CR | LF | CRLF
  deriving (Eq, Ord, Show)

newlineString :: Newline -> String
newlineString n =
  case n of
    CR -> "\r"
    LF -> "\n"
    CRLF -> "\r\n"

