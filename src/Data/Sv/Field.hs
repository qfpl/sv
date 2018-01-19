{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Datatype for a single field or "cell" of a CSV file
module Data.Sv.Field (
    Field (Unquoted, Quoted)
  , SpacedField
  , HasFields (fields)
  , AsField (_Field, _Unquoted, _Quoted)
  , foldField
  , FieldContents (fieldContents)
  , expand
) where

import Control.Lens        (Prism', Traversal', prism, review)
import Data.Bifunctor      (first)
import Data.Bifoldable     (Bifoldable (bifoldMap))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable       (Foldable (foldMap))
import Data.Functor        (Functor (fmap))
import Data.Monoid         (Monoid)
import Data.Profunctor     (Profunctor (dimap))
import Data.Text           (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder as TB (Builder, fromText, fromLazyText, toLazyText)
import Data.Traversable    (Traversable (traverse))

import Text.Babel
import Text.Escaped        (Escaped (Escaped), Escaped')
import Text.Quote          (Quote, quoteChar)
import Text.Space          (Spaced)

-- | A @Field'@ is a single cell from a CSV document.
--   Its value is either surrounded by quotes (@Quoted@), or it is
--   @Unquoted@.
data Field s =
    Unquoted s
  | Quoted Quote (Escaped' s)
  deriving (Eq, Ord, Show)

type SpacedField a = Spaced (Field a)

-- | Classy prisms for 'Field'
class HasFields c s => AsField c s | c -> s where
  _Field :: Prism' c (Field s)
  _Unquoted :: Prism' c s
  _Quoted :: Prism' c (Quote, Escaped' s)
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

class HasFields c s | c -> s where
  fields :: Traversal' c (Field s)

instance HasFields (Field s) s where
  fields = id

-- | The catamorphism for @Field'@
foldField :: (s -> b) -> ((Quote, Escaped' s) -> b) -> Field s -> b
foldField u q fi = case fi of
  Unquoted s -> u s
  Quoted a b -> q (a,b)

{-
-- | 'unspacedField' is a convenient constructor for quoted fields with
--   no spaces surrounding the quotes.
quotedField :: Quote -> s -> Field s
quotedField q = Quoted q . noEscape s
-}

-- | Expands a Quoted, which is compact with all quotes the same, into a
-- WithEscapes Quote, which is often a useful representation, particularly
-- because of its instances of Bitraversable and friends.
expand :: Field a -> Escaped Quote a
expand f = case f of
  Unquoted a -> Escaped [Right a]
  Quoted q v -> first (const q) v

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

-- | Overloads the values in fields, which are stringy and may contain escape
-- sequences.
class Textual a => FieldContents a where
  -- | Extracts the value from a field, expanding escape sequences.
  --
  -- eg. the field @Escaped quote -> "" <-@ would become @Escaped quote -> " <-@
  fieldContents :: Field a -> a

instance FieldContents String where
  fieldContents (Unquoted a) = a
  fieldContents (Quoted q v) =
    let c = review quoteChar q
    in  bifoldMap (const [c]) id v

instance FieldContents TB.Builder where
  fieldContents = expandQuotesTB

instance FieldContents Lazy.Text where
  fieldContents = dimap (fmap TB.fromLazyText) TB.toLazyText expandQuotesTB

instance FieldContents Text where
  fieldContents = dimap (fmap TB.fromText) (Lazy.toStrict . TB.toLazyText) expandQuotesTB

instance FieldContents BS.Builder where
  fieldContents = expandQuotesBSB

instance FieldContents BS.ByteString where
  fieldContents = dimap (fmap BS.byteString) (LBS.toStrict . BS.toLazyByteString) expandQuotesBSB

instance FieldContents LBS.ByteString where
  fieldContents = dimap (fmap BS.lazyByteString) BS.toLazyByteString expandQuotesBSB

expandQuotes :: Monoid a => (Quote -> a) -> Field a -> a
expandQuotes _  (Unquoted a) = a
expandQuotes qb (Quoted q v) =
  bifoldMap (const (qb q)) id v

expandQuotesBSB :: Field BS.Builder -> BS.Builder
expandQuotesBSB =
  expandQuotes (BS.char7 . review quoteChar)

expandQuotesTB :: Field TB.Builder -> TB.Builder
expandQuotesTB =
  expandQuotes (singleton . review quoteChar)
