-- | A sum type for line endings
module Text.Newline (
  Newline (CR, LF, CRLF)
  , newlineString
) where

import Data.String (IsString (fromString))

-- | Sum type for line endings
data Newline =
  CR | LF | CRLF
  deriving (Eq, Ord, Show)

-- | @newlineStirng@ produces the textual representation of a @Newline@
-- TODO replace this with a prism
newlineString :: IsString s => Newline -> s
newlineString n =
  fromString $ case n of
    CR -> "\r"
    LF -> "\n"
    CRLF -> "\r\n"

