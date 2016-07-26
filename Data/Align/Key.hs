-----------------------------------------------------------------------------
-- | Module     :  Data.Aligned.Key
--
-- 'These'-based zipping and unzipping of indexed functors.
module Data.Align.Key (
    AlignWithKey (..)
    ) where

import Data.Key (Key, Keyed (..))
import Data.Vector.Instances ()

import Data.Align
import Data.These

-- Instances
--import Control.Applicative  (ZipList)
import Data.Hashable        (Hashable)
import Data.HashMap.Strict  (HashMap)
import Data.IntMap          (IntMap)
import Data.Map             (Map)
import Data.Sequence        (Seq)
import Data.Vector          (Vector)

-- | Keyed version of 'Align'.
class (Keyed f, Align f) => AlignWithKey f where
    -- | Analogous to @'alignWith'@, but also provides an index.
    alignWithKey :: (Key f -> These a b -> c) -> f a -> f b -> f c
    alignWithKey f a b = mapWithKey f (align a b)

instance AlignWithKey Maybe
instance AlignWithKey []
--instance AlignWithKey ZipList
instance AlignWithKey Seq
instance AlignWithKey IntMap
instance Ord k => AlignWithKey (Map k)
instance (Eq k, Hashable k) => AlignWithKey (HashMap k)
instance AlignWithKey Vector
