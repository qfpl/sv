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

import Control.Lens     (Prism', prism')
import Data.Text        (Text)
import qualified Data.Text as Text

import Text.Between     (Between, betwixt, _value)

data HorizontalSpace =
  Space
  | Tab
  deriving (Eq, Ord, Show)

-- TODO Others?

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

