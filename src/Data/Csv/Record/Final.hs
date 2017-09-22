{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Csv.Record.Final (
    FinalRecord (FinalRecord, _maybeNer)
  , HasFinalRecord (finalRecord, maybeNer)
  , final
  , noFinal
  , singleFinal
  , quotedFinal
) where

import Control.Lens       ((^.), Lens', iso, view)
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text          (Text)
import Data.Text1         (Text1, IsText1 (packed1))
import Data.Traversable   (Traversable (traverse))
import Text.Quote         (Quote, Quoted (Quoted))

import Data.Csv.Field     (Field (UnquotedF, QuotedF))
import Data.Csv.Record.NonEmpty (NonEmptyRecord (SingleFieldNER))
import Text.Between       (betwixt)
import Text.Escaped       (noEscape)

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
