{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- | Module     :  Data.Align.Indexed
--
-- 'These'-based zipping and unzipping of indexed functors.
--
-- @since 0.7.6
module Data.Align.Indexed (
    AlignWithIndex (..),
    ) where

import Control.Lens          (FunctorWithIndex)
import Data.Vector.Instances ()

import Data.Align
import Data.These

-- Instances
import Control.Applicative (ZipList)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap         (IntMap)
import Data.Map            (Map)
import Data.Sequence       (Seq)
import Data.Vector         (Vector)

import qualified Data.Align.Key as Key

-- | Keyed version of 'Align'.
--
-- @since 0.7.6
class (FunctorWithIndex i f, Align f) => AlignWithIndex i f | f -> i where
    -- | Analogous to @'alignWith'@, but also provides an index.
    ialign :: (i -> These a b -> c) -> f a -> f b -> f c

instance AlignWithIndex () Maybe where ialign = Key.alignWithKey
instance AlignWithIndex Int [] where ialign = Key.alignWithKey
instance AlignWithIndex Int ZipList where ialign = Key.alignWithKey
instance AlignWithIndex Int Seq where ialign = Key.alignWithKey
instance AlignWithIndex Int IntMap where ialign = Key.alignWithKey
instance Ord k => AlignWithIndex k (Map k) where ialign = Key.alignWithKey
instance (Eq k, Hashable k) => AlignWithIndex k (HashMap k) where ialign = Key.alignWithKey
instance AlignWithIndex Int Vector where ialign = Key.alignWithKey
