{-# LANGUAGE DefaultSignatures #-}
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

A sum type for space characters
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
  , uniform
  , unspaced
  , removeSpaces
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Lens (Lens, Prism', prism, prism')
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | 'HorizontalSpace' is a subset of 'Char'. To move back and forth betwen
-- it and 'Char', 'String', or 'Data.Text.Text', use '_HorizontalSpace'
data HorizontalSpace =
  Space
  | Tab
  deriving (Eq, Ord, Show)

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
type Spaces = V.Vector HorizontalSpace

-- | One space
single :: Spaces
single = V.singleton Space

-- | As many spaces as you'd like
manySpaces :: Int -> Spaces
manySpaces = flip V.replicate Space

-- | One tab
tab :: Spaces
tab = V.singleton Tab

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

-- | Parse 'Text' into 'Spaces', or turn spaces into 'Data.Text.Text'
spacesText :: Prism' Text Spaces
spacesText =
  prism'
    (Text.pack . foldMap (pure . spaceToChar))
    (fmap V.fromList . traverse charToSpace . Text.unpack)

-- | Parse 'String' into 'Spaces', or convert 'Spaces' into 'String'
spacesString :: Prism' String Spaces
spacesString =
  prism'
    (fmap spaceToChar . V.toList)
    (fmap V.fromList . traverse charToSpace)

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
class HasSpaced s t a b | s -> a, t -> b, s b -> t, t a -> s where
  spaced :: Lens s t (Spaced a) (Spaced b)
  after :: (s ~ t) => Lens s t Spaces Spaces
  {-# INLINE after #-}
  before :: (s ~ t) => Lens s t Spaces Spaces
  {-# INLINE before #-}
  spacedValue :: Lens s t a b
  {-# INLINE spacedValue #-}
  default after :: (s ~ t, a ~ b) => Lens s t Spaces Spaces
  after = spaced . after
  default before :: (s ~ t, a ~ b) => Lens s t Spaces Spaces
  before = spaced . before
  default spacedValue :: (s ~ t, a ~ b) => Lens s t a b
  spacedValue = spaced . spacedValue

instance HasSpaced (Spaced a) (Spaced b) a b where
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
-- > Spaced "   " () " " *> Spaced "\t\t\t" () "\t \t" == Spaced "   \t\t\t" () "\t \t "
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

-- | Places its argument in a 'Spaced' with no spaces.
unspaced :: a -> Spaced a
unspaced = uniform mempty

-- | 'uniform' puts the same spacing both before and after something.
uniform :: Spaces -> a -> Spaced a
uniform s a = Spaced s s a

-- | Remove spaces from the argument
removeSpaces :: Spaced a -> Spaced a
removeSpaces = unspaced . _value
