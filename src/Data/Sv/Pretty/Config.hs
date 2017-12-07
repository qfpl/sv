module Data.Sv.Pretty.Config (
  PrettyConfig' (PrettyConfig', separator', quote, newline, space, string)
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

import Data.Sv.Sv   (Separator)
import Text.Newline (Newline, newlineText)
import Text.Space   (Spaces, spaces)
import Text.Quote   (Quote, quoteText)

data PrettyConfig' f s m =
  PrettyConfig' {
    separator' :: f m
  , quote      :: Quote -> m
  , newline    :: Newline -> m
  , space      :: Spaces -> m
  , string     :: s -> m
  }

instance Functor f => Functor (PrettyConfig' f s) where
  fmap f (PrettyConfig' sep q n sp s) =
    PrettyConfig' {
      separator' = fmap f sep
    , quote = fmap f q
    , newline = fmap f n
    , space = fmap f sp
    , string = fmap f s
    }

type PrettyConfig = PrettyConfig' Identity
type PrettyConfigC = PrettyConfig' ((->) Separator)

setSeparator :: PrettyConfigC s m -> Separator -> PrettyConfig s m
setSeparator (PrettyConfig' sep q n sp s) c =
  PrettyConfig' (Identity (sep c)) q n sp s

separator :: PrettyConfig s m -> m
separator = runIdentity . separator'

textConfig :: PrettyConfigC Text Text
textConfig =
  PrettyConfig' {
    separator' = Text.singleton
  , quote = review quoteText
  , newline = newlineText
  , space = review spaces
  , string = id
  }

defaultConfig :: PrettyConfigC Text Text.Builder
defaultConfig = fmap fromText textConfig
