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

newtype Record spc s =
  Record {
    fields :: NonEmpty (MonoField spc s)
  }
  deriving (Eq, Ord, Show)

instance Functor (Record spc) where
  fmap f = Record . fmap (fmap f) . fields

instance Foldable (Record spc) where
  foldMap f = foldMap (foldMap f) . fields

instance Traversable (Record spc) where
  traverse f = fmap Record . traverse (traverse f) . fields

instance Bifunctor Record where
  bimap f g = Record . fmap (bimap f g) . fields

instance Bifoldable Record where
  bifoldMap f g = foldMap (bifoldMap f g) . fields

instance Bitraversable Record where
  bitraverse f g = fmap Record . traverse (bitraverse f g) . fields

data NonEmptyRecord spc s1 s2 =
    SingleFieldNER (Field spc s1 s2)
  | MultiFieldNER (MonoField spc s2) (NonEmpty (MonoField spc s2))
  deriving (Eq, Ord, Show)

instance Functor (NonEmptyRecord n s) where
  fmap = second

instance Foldable (NonEmptyRecord n s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord n s) where
  traverse = bitraverse pure

instance Bifunctor (NonEmptyRecord n) where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap _ g (MultiFieldNER h hs) = MultiFieldNER (fmap g h) (fmap (fmap g) hs)

instance Bifoldable (NonEmptyRecord n) where
  bifoldMap f g (SingleFieldNER r) = bifoldMap f g r
  bifoldMap _ g (MultiFieldNER h hs) = foldMap g h <> foldMap (bifoldMap (const mempty) g) hs

instance Bitraversable (NonEmptyRecord n) where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse _ g (MultiFieldNER h hs) = MultiFieldNER <$> traverse g h <*> traverse (bitraverse pure g) hs


-- | A collection of records, separated and terminated by newlines.
newtype Records spc s =
  Records { getRecords :: Pesarated Newline (Record spc s) }
  deriving (Eq, Ord, Show)

instance Monoid (Records spc s) where
  mempty = Records mempty
  mappend (Records x) (Records y) = Records (x <> y)

instance Functor (Records spc) where
  fmap f = Records . fmap (fmap f) . getRecords

instance Foldable (Records spc) where
  foldMap f = foldMap (foldMap f) . getRecords

instance Traversable (Records spc) where
  traverse f = fmap Records . traverse (traverse f) . getRecords

instance Bifunctor Records where
  bimap f g = Records . fmap (bimap f g) . getRecords

instance Bifoldable Records where
  bifoldMap f g = foldMap (bifoldMap f g) . getRecords

instance Bitraversable Records where
  bitraverse f g = fmap Records . traverse (bitraverse f g) . getRecords

emptyRecords :: Records spc s
emptyRecords = Records (Pesarated (Separated []))

singletonRecords :: Record spc s -> Newline -> Records spc s
singletonRecords s n = Records (Pesarated (Separated [(s,n)]))


-- | The final record in a Csv can be optionally ended with a newline.
--   A FinalRecord is present if the newline is not present, otherwise
--   all records are in the @initialRecords@
newtype FinalRecord spc s1 s2 =
  FinalRecord { unFinal :: Maybe (NonEmptyRecord spc s1 s2) }
  deriving (Eq, Ord, Show)

instance Functor (FinalRecord n a) where
  fmap = second

instance Foldable (FinalRecord n a) where
  foldMap = bifoldMap (const mempty)

instance Traversable (FinalRecord n a) where
  traverse = bitraverse pure

instance Bifunctor (FinalRecord n) where
  bimap f g = FinalRecord . fmap (bimap f g) . unFinal

instance Bifoldable (FinalRecord n) where
  bifoldMap f g = foldMap (bifoldMap f g) . unFinal

instance Bitraversable (FinalRecord n) where
  bitraverse f g = fmap FinalRecord . traverse (bitraverse f g) . unFinal

final :: NonEmptyRecord spc s1 s2 -> FinalRecord spc s1 s2
final = FinalRecord . Just

noFinal :: FinalRecord a b c
noFinal = FinalRecord Nothing

singleFinal :: Char -> String -> FinalRecord a Text1 c
singleFinal c s = final (SingleFieldNER (UnquotedF ((c :| s) ^. packed1)))

quotedFinal :: Quote -> Text -> FinalRecord Text b Text
quotedFinal q s = final (SingleFieldNER (QuotedF (betwixt mempty mempty (Quoted q (noEscape s)))))
