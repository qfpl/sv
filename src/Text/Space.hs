{-# LANGUAGE TypeFamilies #-}

-- | Large chunks of only space characters, represented efficiently as integers
module Text.Space
  ( HorizontalSpace (Space, Tab)
  , Spaces
  , single
  , manySpaces
  , tab
  , spaceChar
  , space
  , spaces
  , Spaced
  , spaced
  , unspace
  )
where

import Control.Lens     (Prism', prism, prism')
import Data.Text        (Text)
import qualified Data.Text as Text

import Data.Sv.Lens.Util (singletonList, singletonText)
import Text.Between     (Between, betwixt, _value)

data HorizontalSpace =
  Space
  | Tab
  deriving (Eq, Ord, Show)
-- TODO Others?

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
  _HorizontalSpace = space

instance (a ~ Char) => AsHorizontalSpace [a] where
  _HorizontalSpace = singletonList . space

instance AsHorizontalSpace Text where
  _HorizontalSpace = singletonText . _HorizontalSpace

type Spaces = [HorizontalSpace]

-- | One space
single :: Spaces
single = [Space]

manySpaces :: Int -> Spaces
manySpaces = flip replicate Space

tab :: Spaces
tab = [Tab]

spaceChar :: HorizontalSpace -> Char
spaceChar Space = ' '
spaceChar Tab = '\t'

space :: Prism' Char HorizontalSpace
space =
  prism' spaceChar $ \c ->
    case c of
      ' '  -> Just Space
      '\t' -> Just Tab
      _    -> Nothing

-- | Parse 'Text' into 'Spaces', or turn spaces into text
spaces :: Prism' Text Spaces
spaces =
  prism'
    (Text.pack . foldMap (pure . spaceChar))
    (foldMap c2s . Text.unpack)

c2s :: Char -> Maybe Spaces
c2s ' ' = Just single
c2s '\t' = Just tab
c2s _   = Nothing

-- | Something between spaces is 'Spaced'
type Spaced = Between Spaces

-- | Alias for @Text.Between.betwixt@
spaced :: Spaces -> Spaces -> a -> Spaced a
spaced = betwixt

unspace :: Spaced a -> Spaced a
unspace = betwixt mempty mempty . _value

