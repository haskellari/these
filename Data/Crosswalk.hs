{-# LANGUAGE CPP #-}
module Data.Crosswalk (
    -- * Crosswalk
    Crosswalk (..),
    -- * Bicrosswalk
    Bicrosswalk (..),
    ) where

import Prelude ()
import Prelude.Compat

import Data.Bifoldable       (Bifoldable (..))
import Data.Bifunctor        (Bifunctor (..))
import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Vector.Generic   (Vector)

import qualified Data.Sequence       as Seq
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG

import Data.Align
import Data.These

-- --------------------------------------------------------------------------
-- | Foldable functors supporting traversal through an alignable
--   functor.
--
--   Minimal definition: @crosswalk@ or @sequenceL@.
--
--   Laws:
--
-- @
-- crosswalk (const nil) = const nil
-- crosswalk f = sequenceL . fmap f
-- @
class (Functor t, Foldable t) => Crosswalk t where
    crosswalk :: (Align f) => (a -> f b) -> t a -> f (t b)
    crosswalk f = sequenceL . fmap f

    sequenceL :: (Align f) => t (f a) -> f (t a)
    sequenceL = crosswalk id

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL crosswalk | sequenceL #-}
#endif

instance Crosswalk Identity where
    crosswalk f (Identity a) = fmap Identity (f a)

instance Crosswalk Maybe where
    crosswalk _ Nothing = nil
    crosswalk f (Just a) = Just <$> f a

instance Crosswalk [] where
    crosswalk _ [] = nil
    crosswalk f (x:xs) = alignWith cons (f x) (crosswalk f xs)
      where cons = these pure id (:)

instance Crosswalk Seq.Seq where
    crosswalk f = foldr (alignWith cons . f) nil where
        cons = these Seq.singleton id (Seq.<|)

instance Crosswalk (These a) where
    crosswalk _ (This _) = nil
    crosswalk f (That x) = That <$> f x
    crosswalk f (These a x) = These a <$> f x

crosswalkVector :: (Vector v a, Vector v b, Align f)
    => (a -> f b) -> v a -> f (v b)
crosswalkVector f = fmap VG.fromList . VG.foldr (alignWith cons . f) nil where
    cons = these pure id (:)

instance Crosswalk V.Vector where
    crosswalk = crosswalkVector

-- | @since 0.7.5
instance Crosswalk ((,) a) where
    crosswalk fun (a, x) = fmap ((,) a) (fun x)

-- can't (shouldn't) do longer tuples until there are Functor and Foldable
-- instances for them

-- | @since 0.7.5
instance (Crosswalk f, Crosswalk g) => Crosswalk (Compose f g) where
    crosswalk f = id
        . fmap Compose -- can't coerce: maybe the Align-able thing has role nominal
        . crosswalk (crosswalk f)
        . getCompose

-- --------------------------------------------------------------------------
-- | Bifoldable bifunctors supporting traversal through an alignable
--   functor.
--
--   Minimal definition: @bicrosswalk@ or @bisequenceL@.
--
--   Laws:
--
-- @
-- bicrosswalk (const empty) (const empty) = const empty
-- bicrosswalk f g = bisequenceL . bimap f g
-- @
class (Bifunctor t, Bifoldable t) => Bicrosswalk t where
    bicrosswalk :: (Align f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
    bicrosswalk f g = bisequenceL . bimap f g

    bisequenceL :: (Align f) => t (f a) (f b) -> f (t a b)
    bisequenceL = bicrosswalk id id

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL bicrosswalk | bisequenceL #-}
#endif


instance Bicrosswalk Either where
    bicrosswalk f _ (Left x)  = Left  <$> f x
    bicrosswalk _ g (Right x) = Right <$> g x

instance Bicrosswalk These where
    bicrosswalk f _ (This x) = This <$> f x
    bicrosswalk _ g (That x) = That <$> g x
    bicrosswalk f g (These x y) = align (f x) (g y)
