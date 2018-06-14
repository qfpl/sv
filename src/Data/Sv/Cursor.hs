module Data.Sv.Cursor where

import Data.Sv.Cursor.Separator (Separator, HasSeparator (separator), comma)
import Data.Sv.Structure.Headedness (Headedness (Headed), HasHeadedness (headedness))

-- | The default separator
defaultSeparator :: Separator
defaultSeparator = comma

-- | The default is that a header is present.
defaultHeadedness :: Headedness
defaultHeadedness = Headed

-- | A 'ParseOptions' informs the parser how to parse your file.
--
-- A default is provided as 'defaultParseOptions', seen below.
data ParseOptions =
  ParseOptions {
  -- | Which separator does the file use? Usually this is 'comma', but it can
  -- also be 'pipe', or any other 'Char' ('Separator' = 'Char')
    _separator :: Separator

  -- | Whether there is a header row with column names or not.
  , _headedness :: Headedness
  }

defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions defaultSeparator defaultHeadedness

instance HasHeadedness ParseOptions where
  headedness f (ParseOptions s h) = ParseOptions s <$> f h

instance HasSeparator ParseOptions where
  separator f (ParseOptions s h) = (\y -> ParseOptions y h) <$> f s
