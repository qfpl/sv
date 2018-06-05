{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Text.Newline
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

A sum type for line endings
-}

module Text.Newline (
  Newline (CR, LF, CRLF)
  , AsNewline(_Newline, _CR, _LF, _CRLF)
  , newlineToString
  , parseNewline
) where

import Control.DeepSeq (NFData (rnf))
import Control.Lens (Prism', prism, prism')
import Data.String (IsString (fromString))
import Data.Text (Text)

-- | 'Newline' is a sum type for line endings
data Newline =
  -- | > "\r"
  CR
  -- | > "\n"
  | LF
  -- | > "\rn"
  | CRLF
  deriving (Eq, Ord, Show)

instance NFData Newline where
  rnf x = seq x ()

-- | 'AsNewline' is a classy prism for 'Newline'
class AsNewline r where
  _Newline :: Prism' r Newline
  _CR :: Prism' r ()
  _LF :: Prism' r ()
  _CRLF :: Prism' r ()
  _CR = _Newline . _CR
  _LF = _Newline . _LF
  _CRLF = _Newline . _CRLF

instance AsNewline Newline where
  _Newline = id
  _CR =
    prism (const CR) $ \x -> case x of
      CR -> Right ()
      _  -> Left x
  _LF =
    prism (const LF) $ \x -> case x of
      LF -> Right ()
      _  -> Left x
  _CRLF =
    prism (const CRLF) $ \x -> case x of
      CRLF -> Right ()
      _    -> Left x

instance AsNewline Text where
  _Newline = prism' newlineToString parseNewline

-- | Convert a 'Newline' to a 'String'. Since this uses 'Data.String.IsString',
-- it works for other data types, like 'Data.Text.Text' or
-- 'Data.ByteString.ByteString'.
newlineToString :: IsString s => Newline -> s
newlineToString n = fromString $
  case n of
    CR -> "\r"
    LF -> "\n"
    CRLF -> "\r\n"

-- | Try to parse text into a 'Newline'
parseNewline :: Text -> Maybe Newline
parseNewline ""   = Nothing
parseNewline "\r" = Just CR
parseNewline "\n" = Just LF
parseNewline "\r\n" = Just CRLF
parseNewline _ = Nothing

