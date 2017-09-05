module Data.CSV.CSV where

import Control.Lens       ((^.))
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid        ((<>))
import Data.Text          (Text)
import Data.Text.Lens     (IsText (packed))
import Data.Text1         (Text1, IsText1 (packed1))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Field     (Field (UnquotedF, QuotedF))
import Data.CSV.Record    (NonEmptyRecord (SingleFieldNER), Record)
import Data.NonEmptyString (NonEmptyString)
import Data.Separated     (Pesarated (Pesarated), Separated (Separated))
import Text.Between       (betwixt)
import Text.Newline       (Newline)
import Text.Quote         (Quote, Quoted (Quoted), Escaped, noEscape)

-- | Whitespace-preserving CSV data type
data CSV spc s1 s2 =
  CSV {
    separator :: Char
  , initialRecords :: Records spc s2
  , finalRecord :: FinalRecord spc s1 s2
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> FinalRecord spc s1 s2 -> Records spc s2 -> CSV spc s1 s2
mkCsv c ns rs = CSV c rs ns

mkCsv' :: Char -> FinalRecord spc s1 s2 -> Pesarated Newline (Record spc s2) -> CSV spc s1 s2
mkCsv' c ns = mkCsv c ns . Records

instance Functor (CSV n spc) where
  fmap = second

instance Foldable (CSV n spc) where
  foldMap = bifoldMap (const mempty)

instance Traversable (CSV n spc) where
  traverse = bitraverse pure

instance Bifunctor (CSV n) where
  bimap f g (CSV s rs e) = CSV s (fmap g rs) (bimap f g e)

instance Bifoldable (CSV n) where
  bifoldMap f g (CSV _ rs e) = foldMap g rs <> bifoldMap f g e

instance Bitraversable (CSV n) where
  bitraverse f g (CSV s rs e) = CSV s <$> traverse g rs <*> bitraverse f g e


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


-- | The final record in a CSV can be optionally ended with a newline.
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

