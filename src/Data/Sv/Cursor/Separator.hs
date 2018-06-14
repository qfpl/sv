module Data.Sv.Cursor.Separator where

import Control.Lens (Lens')
import GHC.Word (Word8)

-- | By what are your values separated? The answer is often 'comma', but not always.
--
-- A 'Separator' is just a 'Word8'. It could be a sum type instead, since it
-- will usually be comma or pipe, but our preference has been to be open here
-- so that you can use whatever you'd like.
type Separator = Word8

-- | The venerable comma separator. Used for CSV documents.
comma :: Separator
comma = 44

-- | The pipe separator. Used for PSV documents.
pipe :: Separator
pipe = 124

-- | Tab is a separator too - why not?
tab :: Separator
tab = 9

-- | Classy lens for 'Separator'
class HasSeparator c where
  separator :: Lens' c Separator

instance HasSeparator Word8 where
  separator = id
  {-# INLINE separator #-}
