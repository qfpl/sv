{-# LANGUAGE TypeFamilies #-}

-- | Large chunks of only space characters, represented efficiently as integers
module Text.Space
  ( HorizontalSpace (Space, Tab)
  , AsHorizontalSpace (_HorizontalSpace, _Space, _Tab)
  , Spaces
  , single
  , manySpaces
  , tab
  , spaceToChar
  , charToSpace
  , spaces
  , Spaced
  , spaced
  , unspaced
  , unspace
  )
where

import Control.Lens     (Prism', prism, prism')
import Data.Text        (Text)
import qualified Data.Text as Text

import Data.Sv.Lens.Util (singletonList, singletonText)
import Text.Between     (Between, betwixt, _value)

-- | 'HorizontalSpace' is a subset of 'Char'. To move back and forth betwen
-- it and 'Char', 'String', or 'Text', use '_HorizontalSpace'
data HorizontalSpace =
  Space
  | Tab
  deriving (Eq, Ord, Show)
-- TODO Others?

-- | Classy prisms for 'HorizontalSpace's
class AsHorizontalSpace r where
  _HorizontalSpace :: Prism' r HorizontalSpace
  _Space :: Prism' r ()
  _Tab :: Prism' r ()
  _Space = _HorizontalSpace . _Space
  _Tab = _HorizontalSpace . _Tab

instance AsHorizontalSpace HorizontalSpace where
  _HorizontalSpace = id
  _Space =
    prism (const Space) $ \x ->
      case x of
        Space -> Right ()
        _     -> Left x
  _Tab =
    prism (const Tab) $ \x ->
      case x of
        Tab -> Right ()
        _   -> Left x

instance AsHorizontalSpace Char where
  _HorizontalSpace = prism' spaceToChar charToSpace

instance (a ~ Char) => AsHorizontalSpace [a] where
  _HorizontalSpace = singletonList . _HorizontalSpace

instance AsHorizontalSpace Text where
  _HorizontalSpace = singletonText . _HorizontalSpace

-- | Helpful alias for lists of 'Space's
type Spaces = [HorizontalSpace]

-- | One space
single :: Spaces
single = [Space]

-- | As many spaces as you'd like
manySpaces :: Int -> Spaces
manySpaces = flip replicate Space

-- | One tab
tab :: Spaces
tab = [Tab]

-- | Turn a 'Space' into a 'Char'. To go the other way, see 'charToSpace'
spaceToChar :: HorizontalSpace -> Char
spaceToChar Space = ' '
spaceToChar Tab = '\t'

-- | Try to turn a 'Char' into a Space. To go the other way, see 'spaceToChar'
charToSpace :: Char -> Maybe HorizontalSpace
charToSpace c = case c of
  ' '  -> Just Space
  '\t' -> Just Tab
  _    -> Nothing

-- | Parse 'Text' into 'Spaces', or turn spaces into text
spaces :: Prism' Text Spaces
spaces =
  prism'
    (Text.pack . foldMap (pure . spaceToChar))
    (foldMap c2s . Text.unpack)

c2s :: Char -> Maybe Spaces
c2s ' ' = Just single
c2s '\t' = Just tab
c2s _   = Nothing

-- | Something between spaces is 'Spaced'
type Spaced = Between Spaces

-- | Put the given argument between the given spaces.
-- Alias for @Text.Between.betwixt@
spaced :: Spaces -> Spaces -> a -> Spaced a
spaced = betwixt

-- | Put the given argument between no spaces
unspaced :: a -> Spaced a
unspaced = betwixt mempty mempty

-- | Remove spaces from the argument
unspace :: Spaced a -> Spaced a
unspace = unspaced . _value
