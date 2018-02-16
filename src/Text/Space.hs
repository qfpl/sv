{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Text.Space
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

Large chunks of only space characters, represented efficiently as integers
-}

module Text.Space
  ( HorizontalSpace (Space, Tab)
  , AsHorizontalSpace (_HorizontalSpace, _Space, _Tab)
  , Spaces
  , single
  , manySpaces
  , tab
  , spaceToChar
  , charToSpace
  , spacesText
  , spacesString
  , Spaced (Spaced, _before, _after, _value)
  , HasSpaced (spaced, spacedValue, before, after)
  , betwixt
  , noSpaces
  , unspaced
  , removeSpaces
  )
where

import Control.DeepSeq  (NFData (rnf))
import Control.Lens     (Lens', Prism', prism, prism')
import Data.Semigroup   (Semigroup ((<>)))
import Data.Text        (Text)
import qualified Data.Text as Text
import GHC.Generics     (Generic)

-- | 'HorizontalSpace' is a subset of 'Char'. To move back and forth betwen
-- it and 'Char', 'String', or 'Text', use '_HorizontalSpace'
data HorizontalSpace =
  Space
  | Tab
  deriving (Eq, Ord, Show)
-- TODO Others?

instance NFData HorizontalSpace where
  rnf x = seq x ()

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

-- | Parse 'Text' into 'Spaces', or turn spaces into 'Text'
spacesText :: Prism' Text Spaces
spacesText =
  prism'
    (Text.pack . foldMap (pure . spaceToChar))
    (foldMap c2s . Text.unpack)

-- | Parse 'String' into 'Spaces', or convert 'Spaces' into 'String'
spacesString :: Prism' String Spaces
spacesString =
  prism'
    (fmap spaceToChar)
    (foldMap c2s)

c2s :: Char -> Maybe Spaces
c2s ' ' = Just single
c2s '\t' = Just tab
c2s _   = Nothing

-- | 'Spaced' is a value with zero or many horizontal spaces around it on
-- both sides.
data Spaced a =
  Spaced {
    _before :: Spaces
  , _after :: Spaces
  , _value :: a
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (Spaced a)

-- | Classy lenses for 'Spaced'
class HasSpaced c s | c -> s where
  spaced :: Lens' c (Spaced s)
  after :: Lens' c Spaces
  {-# INLINE after #-}
  before :: Lens' c Spaces
  {-# INLINE before #-}
  spacedValue :: Lens' c s
  {-# INLINE spacedValue #-}
  after = spaced . after
  before = spaced . before
  spacedValue = spaced . spacedValue

instance HasSpaced (Spaced a) a where
  {-# INLINE after #-}
  {-# INLINE before #-}
  {-# INLINE spacedValue #-}
  spaced = id
  before f (Spaced x y z) = fmap (\w -> Spaced w y z) (f x)
  spacedValue f (Spaced x y z) = fmap (Spaced x y) (f z)
  after f (Spaced x y z) = fmap (\w -> Spaced x w z) (f y)

instance Functor Spaced where
  fmap f (Spaced b t a) = Spaced b t (f a)

-- | Appends the right parameter on the inside of the left parameter
--
-- Eg. Spaced "   " () " " *> Spaced "\t\t\t" () "\t \t" == Spaced "   \t\t\t" () "\t \t "
instance Applicative Spaced where
  pure = unspaced
  Spaced b t f <*> Spaced b' t' a = Spaced (b <> b') (t' <> t) (f a)

instance Foldable Spaced where
  foldMap f = f . _value

instance Traversable Spaced where
  traverse f (Spaced b t a) = fmap (Spaced b t) (f a)

-- | 'betwixt' is just the constructor for 'Spaced' with a different
-- argument order, which is sometimes useful.
betwixt :: Spaces -> a -> Spaces -> Spaced a
betwixt b a t = Spaced b t a

-- | 'unspaced a' is an 'a' that is between two empty 's's.
unspaced :: a -> Spaced a
unspaced = uniform mempty

-- | `uniform spaces a` is 'a' between two of the same 'spaces'.
uniform :: Spaces -> a -> Spaced a
uniform s a = Spaced s s a

-- | Put the given argument between no spaces
noSpaces :: a -> Spaced a
noSpaces = uniform mempty

-- | Remove spaces from the argument
removeSpaces :: Spaced a -> Spaced a
removeSpaces = unspaced . _value
