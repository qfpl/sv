{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This file contains datatypes for Records. A record is a "line" or "row"
-- of a CSV document
module Data.Csv.Record (
  Record (Record, _fields)
  , NonEmptyRecord (SingleFieldNER, MultiFieldNER)
  -- Optics
  , HasRecord (record, fields)
  , HasRecords (records, theRecords)
  , Records (Records, _theRecords)
  , emptyRecords
  , singletonRecords
  , multiFieldNER
  , FinalRecord (FinalRecord, _maybeNer)
  , HasFinalRecord (finalRecord, maybeNer)
  , final
  , noFinal
  , singleFinal
  , quotedFinal
) where

import Control.Lens       ((^.), Lens', Prism', prism, Iso, iso, view)
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.AtLeastTwo (AtLeastTwo (AtLeastTwo))
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
    _fields :: NonEmpty (MonoField s)
  }
  deriving (Eq, Ord, Show)

fields' :: Iso (Record s) (Record a) (NonEmpty (MonoField s)) (NonEmpty (MonoField a))
fields' = iso _fields Record

class HasRecord s t | s -> t where
  record :: Lens' s (Record t)
  fields :: Lens' s (NonEmpty (MonoField t))
  {-# INLINE fields #-}
  fields = record . fields

instance HasRecord (Record s) s where
  record = id
  {-# INLINE fields #-}
  fields = fields'

instance Functor Record where
  fmap f = Record . fmap (fmap f) . _fields

instance Foldable Record where
  foldMap f = foldMap (foldMap f) . _fields

instance Traversable Record where
  traverse f = fmap Record . traverse (traverse f) . _fields

-- | A records which is guaranteed not the be the empty string.
-- `s1` is the non-empty string type (Text1 or NonEmpty Char) and `s1` is the
-- corresponding empty-capable type (Text or [Char])
data NonEmptyRecord s1 s2 =
    SingleFieldNER (Field s1 s2)
  | MultiFieldNER (AtLeastTwo (MonoField s2))
  deriving (Eq, Ord, Show)

class AsNonEmptyRecord r s1 s2 | r -> s1 s2 where
  _NonEmptyRecord :: Prism' r (NonEmptyRecord s1 s2)
  _SingleFieldNER :: Prism' r (Field s1 s2)
  _MultiFieldNER :: Prism' r (AtLeastTwo (MonoField s2))
  _SingleFieldNER = _NonEmptyRecord . _SingleFieldNER
  _MultiFieldNER = _NonEmptyRecord . _MultiFieldNER

instance AsNonEmptyRecord (NonEmptyRecord s1 s2) s1 s2 where
  _NonEmptyRecord = id
  _SingleFieldNER =
    prism SingleFieldNER $ \x -> case x of
      SingleFieldNER y -> Right y
      _ -> Left x
  _MultiFieldNER =
    prism MultiFieldNER $ \x -> case x of
      MultiFieldNER y -> Right y
      _ -> Left x

instance Functor (NonEmptyRecord s) where
  fmap = second

instance Foldable (NonEmptyRecord s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord s) where
  traverse = bitraverse pure

instance Bifunctor NonEmptyRecord where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap _ g (MultiFieldNER hs) = MultiFieldNER (fmap (fmap g) hs)

instance Bifoldable NonEmptyRecord where
  bifoldMap f g (SingleFieldNER r) = bifoldMap f g r
  bifoldMap _ g (MultiFieldNER hs) = foldMap (foldMap g) hs

instance Bitraversable NonEmptyRecord where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse _ g (MultiFieldNER hs) = MultiFieldNER <$> traverse (traverse g) hs

multiFieldNER :: MonoField a -> NonEmpty (MonoField a) -> NonEmptyRecord s a
multiFieldNER x xs = MultiFieldNER (AtLeastTwo x xs)

-- | A collection of records, separated and terminated by newlines.
newtype Records s =
  Records { _theRecords :: Pesarated Newline (Record s) }
  deriving (Eq, Ord, Show)

class HasRecords s a | s -> a where
  records :: Lens' s (Records a)
  theRecords :: Lens' s (Pesarated Newline (Record a))
  {-# INLINE theRecords #-}
  theRecords = records . theRecords

instance HasRecords (Records s) s where
  {-# INLINE theRecords #-}
  records = id
  theRecords = iso _theRecords Records

instance Monoid (Records s) where
  mempty = Records mempty
  mappend (Records x) (Records y) = Records (x <> y)

instance Functor Records where
  fmap f = Records . fmap (fmap f) . view theRecords

instance Foldable Records where
  foldMap f = foldMap (foldMap f) . view theRecords

instance Traversable Records where
  traverse f = fmap Records . traverse (traverse f) . view theRecords

emptyRecords :: Records s
emptyRecords = Records mempty

singletonRecords :: Record s -> Newline -> Records s
singletonRecords s n = Records (Pesarated (Separated [(s,n)]))


-- | The final record in a Csv can be optionally ended with a newline.
--   A FinalRecord is present if the newline is not present, otherwise
--   all records are in the @initialRecords@
newtype FinalRecord s1 s2 =
  FinalRecord { _maybeNer :: Maybe (NonEmptyRecord s1 s2) }
  deriving (Eq, Ord, Show)

class HasFinalRecord c s1 s2 | c -> s1 s2 where
  finalRecord :: Lens' c (FinalRecord s1 s2)
  maybeNer :: Lens' c (Maybe (NonEmptyRecord s1 s2))
  {-# INLINE maybeNer #-}
  maybeNer = finalRecord . maybeNer
instance HasFinalRecord (FinalRecord s1 s2) s1 s2 where
  {-# INLINE maybeNer #-}
  finalRecord = id
  maybeNer = iso _maybeNer FinalRecord


instance Functor (FinalRecord a) where
  fmap = second

instance Foldable (FinalRecord a) where
  foldMap = bifoldMap (const mempty)

instance Traversable (FinalRecord a) where
  traverse = bitraverse pure

instance Bifunctor FinalRecord where
  bimap f g = FinalRecord . fmap (bimap f g) . view maybeNer

instance Bifoldable FinalRecord where
  bifoldMap f g = foldMap (bifoldMap f g) . view maybeNer

instance Bitraversable FinalRecord where
  bitraverse f g = fmap FinalRecord . traverse (bitraverse f g) . view maybeNer

final :: NonEmptyRecord s1 s2 -> FinalRecord s1 s2
final = FinalRecord . Just

noFinal :: FinalRecord a b
noFinal = FinalRecord Nothing

singleFinal :: Char -> String -> FinalRecord Text1 c
singleFinal c s = final (SingleFieldNER (UnquotedF ((c :| s) ^. packed1)))

quotedFinal :: Quote -> Text -> FinalRecord b Text
quotedFinal q s = final (SingleFieldNER (QuotedF (betwixt mempty mempty (Quoted q (noEscape s)))))

