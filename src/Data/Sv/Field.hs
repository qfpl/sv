{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Datatype for a single field or "cell" of a CSV file
module Data.Sv.Field (
    Field (Unquoted, Quoted)
  , SpacedField
  , Spaced (Spaced)
  , HasFields (fields)
  , AsField (_Field, _Unquoted, _Quoted)
  , foldField
  , fieldContents
) where

import Control.Lens        (Lens, Prism', Traversal', lens, prism)
import Data.Foldable       (Foldable (foldMap))
import Data.Functor        (Functor (fmap))
import Data.Traversable    (Traversable (traverse))

import Text.Escaped        (Unescaped (Unescaped, getUnescaped))
import Text.Quote          (Quote)
import Text.Space          (Spaced (Spaced))

-- | A 'Field' is a single cell from a CSV document.
--
-- Its value is either 'Quoted', which indicates the type of quote
-- surrounding the value, or it is 'Unquoted', containing only the value.
data Field s =
    Unquoted s
  | Quoted Quote (Unescaped s)
  deriving (Eq, Ord, Show)

-- | 'Field's are very often surrounded by spaces
type SpacedField a = Spaced (Field a)

-- | Classy prisms for 'Field'
class HasFields c s => AsField c s | c -> s where
  _Field :: Prism' c (Field s)
  _Unquoted :: Prism' c s
  _Quoted :: Prism' c (Quote, Unescaped s)
  _Unquoted = _Field . _Unquoted
  _Quoted = _Field . _Quoted

instance AsField (Field s) s where
  _Field = id
  _Unquoted = prism Unquoted
    (\x -> case x of
      Unquoted y -> Right y
      _          -> Left x
    )
  _Quoted = prism (uncurry Quoted)
    (\x -> case x of
      Quoted y z -> Right (y,z)
      _          -> Left x
    )

-- | Classy 'Traversal'' for things containing 'Field's
class HasFields c s | c -> s where
  fields :: Traversal' c (Field s)

instance HasFields (Field s) s where
  fields = id

-- | The catamorphism for @Field'@
foldField :: (s -> b) -> ((Quote, Unescaped s) -> b) -> Field s -> b
foldField u q fi = case fi of
  Unquoted s -> u s
  Quoted a b -> q (a,b)

instance Functor Field where
  fmap f fi = case fi of
    Unquoted s -> Unquoted (f s)
    Quoted q v -> Quoted q (fmap f v)

instance Foldable Field where
  foldMap f fi = case fi of
    Unquoted s -> f s
    Quoted _ v -> foldMap f v

instance Traversable Field where
  traverse f fi = case fi of
    Unquoted s -> Unquoted <$> f s
    Quoted q v -> Quoted q <$> traverse f v

fieldContents :: Lens (Field s) (Field t) s t
fieldContents =
  lens (foldField id (getUnescaped.snd)) $ \f b -> case f of
    Unquoted _ -> Unquoted b
    Quoted q _ -> Quoted q (Unescaped b)
