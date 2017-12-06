{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Datatype for a single field or "cell" of a CSV file
module Data.Csv.Field (
    Field (UnquotedF, QuotedF)
  , foldField
  , unspacedField
  , FieldContents (expandQuotes)
  , fieldContents
) where

import Control.Lens        (review, view)
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
import Text.Between        (value)
import Text.Escaped        (noEscape)
import Text.Space          (Spaced)
import Text.Quote          (Quote, Quoted (Quoted), quoteChar)

-- | A @Field'@ is a single cell from a CSV document.
--   Its value is either surrounded by quotes (@QuotedF@), or it is
--   @UnquotedF@.
--
--   Any spacing around quotes is preserved.
data Field s =
    UnquotedF s
  | QuotedF (Spaced (Quoted s))
  deriving (Eq, Ord, Show)

-- | The catamorphism for @Field'@
foldField :: (s -> b) -> (Spaced (Quoted s) -> b) -> Field s -> b
foldField u q fi = case fi of
  UnquotedF s -> u s
  QuotedF b -> q b

-- | 'unspacedField' is a convenient constructor for quoted fields with
--   no spaces surrounding the quotes.
unspacedField :: Quote -> s -> Field s
unspacedField q s = QuotedF (pure (Quoted q (noEscape s)))

instance Functor Field where
  fmap f = foldField (UnquotedF . f) (QuotedF . fmap (fmap f))

instance Foldable Field where
  foldMap f = foldField f (foldMap (foldMap f))

instance Traversable Field where
  traverse f =
    foldField (fmap UnquotedF . f) (fmap QuotedF . traverse (traverse f))

class Textual a => FieldContents a where
  expandQuotes :: Quoted a -> a

fieldContents :: FieldContents s => Field s -> s
fieldContents = foldField id (expandQuotes . view value)

instance FieldContents String where
  expandQuotes (Quoted q v) =
    let c = review quoteChar q
    in  bifoldMap (const [c]) id v

instance FieldContents TB.Builder where
  expandQuotes = expandQuotesTB

instance FieldContents Lazy.Text where
  expandQuotes = dimap (fmap TB.fromLazyText) TB.toLazyText expandQuotesTB

instance FieldContents Text where
  expandQuotes = dimap (fmap TB.fromText) (Lazy.toStrict . TB.toLazyText) expandQuotesTB

instance FieldContents BS.Builder where
  expandQuotes = expandQuotesBSB

instance FieldContents BS.ByteString where
  expandQuotes = dimap (fmap BS.byteString) (LBS.toStrict . BS.toLazyByteString) expandQuotesBSB

instance FieldContents LBS.ByteString where
  expandQuotes = dimap (fmap BS.lazyByteString) BS.toLazyByteString expandQuotesBSB

expandQuotes_ :: Monoid a => (Quote -> a) -> Quoted a -> a
expandQuotes_ qb (Quoted q v) =
  bifoldMap (const (qb q)) id v

expandQuotesBSB :: Quoted BS.Builder -> BS.Builder
expandQuotesBSB =
  expandQuotes_ (BS.char7 . review quoteChar)

expandQuotesTB :: Quoted TB.Builder -> TB.Builder
expandQuotesTB =
  expandQuotes_ (singleton . review quoteChar)
