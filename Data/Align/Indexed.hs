{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
-----------------------------------------------------------------------------
-- | Module     :  Data.Align.Indexed
--
-- 'These'-based zipping and unzipping of indexed functors.
module Data.Align.Indexed (
    AlignWithIndex(..)
    ) where

import Control.Lens.Indexed (FunctorWithIndex (..))

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

-- | Indexed version of 'Align'.
class (FunctorWithIndex i f, Align f) => AlignWithIndex i f | f -> i where
    -- | Analogous to @'alignWith'@, but also provides an index.
    ialignWith :: (i -> These a b -> c) -> f a -> f b -> f c
    ialignWith f a b = imap f (align a b)

instance AlignWithIndex () Maybe
instance AlignWithIndex Int []
--instance AlignWithIndex Int ZipList
instance AlignWithIndex Int Seq
instance AlignWithIndex Int IntMap
instance Ord k => AlignWithIndex k (Map k)
instance (Eq k, Hashable k) => AlignWithIndex k (HashMap k)
instance AlignWithIndex Int Vector
