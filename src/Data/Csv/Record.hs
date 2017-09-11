-- | This file contains datatypes for Records. A record is a "line" or "row"
-- of a CSV document
module Data.Csv.Record (
  Record (Record, fields)
  , NonEmptyRecord (SingleFieldNER, MultiFieldNER)
  , Records (Records, getRecords)
  , emptyRecords
  , singletonRecords
  , FinalRecord (FinalRecord, unFinal)
  , final
  , noFinal
  , singleFinal
  , quotedFinal
) where

import Control.Lens       ((^.))
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid        ((<>))
import Data.Separated     (Pesarated (Pesarated), Separated (Separated))
import Data.Text          (Text)
import Data.Text1         (Text1, IsText1 (packed1))
import Data.Traversable   (Traversable (traverse))

import Data.Csv.Field     (Field (UnquotedF, QuotedF), MonoField)
import Text.Between       (betwixt)
import Text.Escaped       (noEscape)
import Text.Newline       (Newline)
import Text.Quote         (Quote, Quoted (Quoted))

-- | A @Record@ is a non-empty collection of Fields, implicitly separated
-- by commas.
newtype Record s =
  Record {
    fields :: NonEmpty (MonoField s)
  }
  deriving (Eq, Ord, Show)

instance Functor Record where
  fmap f = Record . fmap (fmap f) . fields

instance Foldable Record where
  foldMap f = foldMap (foldMap f) . fields

instance Traversable Record where
  traverse f = fmap Record . traverse (traverse f) . fields

data NonEmptyRecord s1 s2 =
    SingleFieldNER (Field s1 s2)
  | MultiFieldNER (MonoField s2) (NonEmpty (MonoField s2))
  deriving (Eq, Ord, Show)

instance Functor (NonEmptyRecord s) where
  fmap = second

instance Foldable (NonEmptyRecord s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord s) where
  traverse = bitraverse pure

instance Bifunctor NonEmptyRecord where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap _ g (MultiFieldNER h hs) = MultiFieldNER (fmap g h) (fmap (fmap g) hs)

instance Bifoldable NonEmptyRecord where
  bifoldMap f g (SingleFieldNER r) = bifoldMap f g r
  bifoldMap _ g (MultiFieldNER h hs) = foldMap g h <> foldMap (foldMap g) hs

instance Bitraversable NonEmptyRecord where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse _ g (MultiFieldNER h hs) = MultiFieldNER <$> traverse g h <*> traverse (traverse g) hs


-- | A collection of records, separated and terminated by newlines.
newtype Records s =
  Records { getRecords :: Pesarated Newline (Record s) }
  deriving (Eq, Ord, Show)

instance Monoid (Records s) where
  mempty = Records mempty
  mappend (Records x) (Records y) = Records (x <> y)

instance Functor Records where
  fmap f = Records . fmap (fmap f) . getRecords

instance Foldable Records where
  foldMap f = foldMap (foldMap f) . getRecords

instance Traversable Records where
  traverse f = fmap Records . traverse (traverse f) . getRecords

emptyRecords :: Records s
emptyRecords = Records mempty

singletonRecords :: Record s -> Newline -> Records s
singletonRecords s n = Records (Pesarated (Separated [(s,n)]))


-- | The final record in a Csv can be optionally ended with a newline.
--   A FinalRecord is present if the newline is not present, otherwise
--   all records are in the @initialRecords@
newtype FinalRecord s1 s2 =
  FinalRecord { unFinal :: Maybe (NonEmptyRecord s1 s2) }
  deriving (Eq, Ord, Show)

instance Functor (FinalRecord a) where
  fmap = second

instance Foldable (FinalRecord a) where
  foldMap = bifoldMap (const mempty)

instance Traversable (FinalRecord a) where
  traverse = bitraverse pure

instance Bifunctor FinalRecord where
  bimap f g = FinalRecord . fmap (bimap f g) . unFinal

instance Bifoldable FinalRecord where
  bifoldMap f g = foldMap (bifoldMap f g) . unFinal

instance Bitraversable FinalRecord where
  bitraverse f g = fmap FinalRecord . traverse (bitraverse f g) . unFinal

final :: NonEmptyRecord s1 s2 -> FinalRecord s1 s2
final = FinalRecord . Just

noFinal :: FinalRecord a b
noFinal = FinalRecord Nothing

singleFinal :: Char -> String -> FinalRecord Text1 c
singleFinal c s = final (SingleFieldNER (UnquotedF ((c :| s) ^. packed1)))

quotedFinal :: Quote -> Text -> FinalRecord b Text
quotedFinal q s = final (SingleFieldNER (QuotedF (betwixt mempty mempty (Quoted q (noEscape s)))))
