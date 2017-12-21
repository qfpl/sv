{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Sv.Config (
  SvConfig (SvConfig, _headedness, _separator, _parsingLib)
, HasSvConfig (svConfig)
, defaultConfig
, orDefault
, Separator
, HasSeparator (separator)
, comma
, pipe
, tab
, Headedness (Unheaded, Headed)
, HasHeadedness (headedness)
, defaultHeadedness
, ParsingLib (Trifecta, Attoparsec)
, HasParsingLib (parsingLib)
, defaultParsingLib
) where

import Control.Lens (Lens', lens)
import Data.Maybe (fromMaybe)

data SvConfig =
  SvConfig {
    _separator :: Separator
  , _headedness :: Headedness
  , _parsingLib :: ParsingLib
  }

class (HasSeparator c, HasHeadedness c, HasParsingLib c) => HasSvConfig c where
  svConfig :: Lens' c SvConfig

instance HasSvConfig SvConfig where
  svConfig = id
  {-# INLINE svConfig #-}

instance HasSeparator SvConfig where
  separator =
    lens _separator (\c s -> c { _separator = s })

instance HasHeadedness SvConfig where
  headedness =
    lens _headedness (\c h -> c { _headedness = h })

instance HasParsingLib SvConfig where
  parsingLib =
    lens _parsingLib (\c p -> c { _parsingLib = p })

defaultConfig :: SvConfig
defaultConfig = SvConfig defaultSeparator defaultHeadedness defaultParsingLib

orDefault :: Maybe SvConfig -> SvConfig
orDefault = fromMaybe defaultConfig

-- | Does the 'Sv' have a 'Header' or not?
data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

class HasHeadedness c where
  headedness :: Lens' c Headedness

instance HasHeadedness Headedness where
  headedness = id

defaultHeadedness :: Headedness
defaultHeadedness = Headed

-- | A 'Separator' is just a 'Char'. It could be a sum type instead, since it
-- will usually be comma or pipe, but our preference has been to be open here
-- so that you can use whatever you'd like.
type Separator = Char

class HasSeparator c where
  separator :: Lens' c Separator

instance HasSeparator Char where
  separator = id
  {-# INLINE separator #-}

-- | The venerable comma separator. Used for CSV documents.
comma :: Separator
comma = ','

-- | The default separator. Alias for 'comma'.
defaultSeparator :: Separator
defaultSeparator = comma

-- | The pipe separator. Used for PSV documents.
pipe :: Separator
pipe = '|'

-- | Tab is a separator too - why not?
tab :: Separator
tab = '\t'

data ParsingLib =
  Trifecta | Attoparsec
  deriving (Eq, Ord, Show)

class HasParsingLib c  where
  parsingLib :: Lens' c ParsingLib

instance HasParsingLib ParsingLib where
  parsingLib = id
  {-# INLINE parsingLib #-}

defaultParsingLib :: ParsingLib
defaultParsingLib = Trifecta
