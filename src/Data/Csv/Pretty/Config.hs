module Data.Csv.Pretty.Config (
  PrettyConfig' (PrettyConfig', separator', quote, newline, space, string1, string2)
  , PrettyConfig
  , PrettyConfigC
  , setSeparator
  , separator
  , textConfig
  , defaultConfig
) where

import Control.Lens             (review)
import Data.Functor.Identity    (Identity (Identity, runIdentity))
import Data.Text                (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Text (Builder, fromText)
import Data.Text1               (Text1, _Text1)

import Text.Newline    (Newline, newlineText)
import Text.Space      (Spaces, spaces)
import Text.Quote      (Quote, quoteText)

data PrettyConfig' f s1 s2 m =
  PrettyConfig' {
    separator' :: f m
  , quote     :: Quote -> m
  , newline   :: Newline -> m
  , space     :: Spaces -> m
  , string1   :: s1 -> m
  , string2   :: s2 -> m
  }

instance Functor f => Functor (PrettyConfig' f s1 s2) where
  fmap f (PrettyConfig' s q n sp s1 s2) =
    PrettyConfig' {
      separator' = fmap f s
    , quote = fmap f q
    , newline = fmap f n
    , space = fmap f sp
    , string1 = fmap f s1
    , string2 = fmap f s2
    }

type PrettyConfig = PrettyConfig' Identity
type PrettyConfigC = PrettyConfig' ((->) Char)

setSeparator :: PrettyConfigC s1 s2 m -> Char -> PrettyConfig s1 s2 m
setSeparator (PrettyConfig' s q n sp s1 s2) c =
  PrettyConfig' (Identity (s c)) q n sp s1 s2

separator :: PrettyConfig s1 s2 m -> m
separator = runIdentity . separator'

textConfig :: PrettyConfigC Text1 Text Text
textConfig =
  PrettyConfig' {
    separator' = Text.singleton
  , quote = review quoteText
  , newline = newlineText
  , space = review spaces
  , string1 = review _Text1
  , string2 = id
  }

defaultConfig :: PrettyConfigC Text1 Text Text.Builder
defaultConfig = fmap fromText textConfig
