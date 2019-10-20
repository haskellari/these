{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | Zipping and aligning of indexed functors.
module Data.Semialign.Indexed (
    SemialignWithIndex (..),
    ZipWithIndex (..),
    ) where

import Prelude hiding (zip, zipWith, repeat)

import Control.Lens (FunctorWithIndex (imap))

import Data.Align
import Data.Zip
import Data.These

-- Instances
import Control.Applicative   (ZipList)
import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity)
import Data.Functor.Product  (Product (..))
import Data.Hashable         (Hashable)
import Data.HashMap.Strict   (HashMap)
import Data.IntMap           (IntMap)
import Data.Map              (Map)
import Data.Sequence         (Seq)
import Data.Vector           (Vector)

import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap       as IntMap
import qualified Data.Map          as Map
import qualified Data.Vector       as V

-- | Indexed version of 'Semialign'.
class (FunctorWithIndex i f, Semialign f) => SemialignWithIndex i f | f -> i where
    -- | Analogous to 'alignWith', but also provides an index.
    ialignWith :: (i -> These a b -> c) -> f a -> f b -> f c
    ialignWith f a b = imap f (align a b)

-- | Indexed version of 'Zip'
class (SemialignWithIndex i f, Zip f) => ZipWithIndex i f | f -> i where
    -- | Analogous to 'zipWith', but also provides an index.
    izipWith :: (i -> a -> b -> c) -> f a -> f b -> f c
    izipWith f a b = imap (uncurry . f) (zip a b)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance SemialignWithIndex () Maybe
instance ZipWithIndex () Maybe
instance SemialignWithIndex Int []
instance ZipWithIndex Int []
instance SemialignWithIndex Int ZipList
instance ZipWithIndex Int ZipList

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance SemialignWithIndex () Identity
instance ZipWithIndex () Identity

instance (SemialignWithIndex i f, SemialignWithIndex j g) => SemialignWithIndex (Either i j) (Product f g) where
    ialignWith f (Pair fa ga) (Pair fb gb) = Pair fc gc where
        fc = ialignWith (f . Left) fa fb
        gc = ialignWith (f . Right) ga gb

instance (ZipWithIndex i f, ZipWithIndex j g) => ZipWithIndex (Either i j) (Product f g) where
    izipWith f (Pair fa ga) (Pair fb gb) = Pair fc gc where
        fc = izipWith (f . Left) fa fb
        gc = izipWith (f . Right) ga gb

instance (SemialignWithIndex i f, SemialignWithIndex j g) => SemialignWithIndex (i, j) (Compose f g) where
    ialignWith f (Compose fga) (Compose fgb) = Compose $ ialignWith g fga fgb where
        g i (This ga)     = imap (\j -> f (i, j) . This) ga
        g i (That gb)     = imap (\j -> f (i, j) . That) gb
        g i (These ga gb) = ialignWith (\j -> f (i, j)) ga gb

instance (ZipWithIndex i f, ZipWithIndex j g) => ZipWithIndex (i, j) (Compose f g) where
    izipWith f (Compose fga) (Compose fgb) = Compose fgc where
        fgc = izipWith (\i -> izipWith (\j -> f (i, j))) fga fgb

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance SemialignWithIndex Int Seq
instance ZipWithIndex Int Seq
instance SemialignWithIndex Int IntMap
instance ZipWithIndex Int IntMap where
    izipWith = IntMap.intersectionWithKey
instance Ord k => SemialignWithIndex k (Map k) where
instance Ord k => ZipWithIndex k (Map k) where
    izipWith = Map.intersectionWithKey

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Eq k, Hashable k) => SemialignWithIndex k (HashMap k) where
instance (Eq k, Hashable k) => ZipWithIndex k (HashMap k) where
    izipWith = HM.intersectionWithKey

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance SemialignWithIndex Int Vector where
instance ZipWithIndex Int Vector where
    izipWith = V.izipWith
