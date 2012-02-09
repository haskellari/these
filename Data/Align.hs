-----------------------------------------------------------------------------
-- | Module     :  Data.Align
--
-- 'These'-based zipping and unzipping of functors with non-uniform
-- shapes, plus traversal of (bi)foldable (bi)functors through said
-- functors.
module Data.Align (
                    Align(..)
                  , Unalign(..)
                  , Crosswalk(..)
                  , Bicrosswalk(..)
                  ) where

-- TODO: More instances..

import Control.Applicative (ZipList(..), pure, (<$>))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.These
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

oops :: String -> a
oops = error . ("Data.Align: internal error: " ++)

-- --------------------------------------------------------------------------
-- | Functors supporting a zip operation that takes the union of
--   non-uniform shapes.
--
--   If your functor is actually a functor from @Kleisli Maybe@ to
--   @Hask@ (so it supports @maybeMap :: (a -> Maybe b) -> f a -> f
--   b@), then an @Align@ instance is making your functor lax monoidal
--   w.r.t. the cartesian monoidal structure on @Kleisli Maybe@,
--   because @These@ is the cartesian product in that category @(a ->
--   Maybe (These b c) ~ (a -> Maybe b, a -> Maybe c))@. This insight
--   is due to rwbarton.
--
--   Minimal definition: @nil@ and either @align@ or @alignWith@.
--
--   Laws:
--
-- @
-- (\`align` nil) = fmap This
-- (nil \`align`) = fmap That
-- join align = fmap (join These)
-- align (f \<$> x) (g \<$> y) = bimap f g \<$> align x y
-- alignWith f a b = f \<$> align a b
-- @
class (Functor f) => Align f where
    nil :: f a

    align :: f a -> f b -> f (These a b)
    align = alignWith id

    alignWith :: (These a b -> c) -> f a -> f b -> f c
    alignWith f a b = f <$> align a b

instance Align Maybe where
    nil = Nothing
    align Nothing Nothing = Nothing
    align (Just a) Nothing = Just (This a)
    align Nothing (Just b) = Just (That b)
    align (Just a) (Just b) = Just (These a b)

instance Align [] where
    nil = []
    align xs [] = This <$> xs
    align [] ys = That <$> ys
    align (x:xs) (y:ys) = These x y : align xs ys

instance Align ZipList where
    nil = ZipList []
    align (ZipList xs) (ZipList ys) = ZipList (align xs ys)

-- could probably be more efficient...
instance Align Seq where
    nil = Seq.empty
    align xs ys =
        case Seq.viewl xs of
            Seq.EmptyL   -> That <$> ys
            x Seq.:< xs' ->
                case Seq.viewl ys of
                    Seq.EmptyL   -> This <$> xs
                    y Seq.:< ys' -> These x y Seq.<| align xs' ys'

instance (Ord k) => Align (Map k) where
    nil = Map.empty
    align m n = Map.unionWith merge (Map.map This m) (Map.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align Map: merge"

instance Align IntMap where
    nil = IntMap.empty
    align m n = IntMap.unionWith merge (IntMap.map This m) (IntMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align IntMap: merge"

-- --------------------------------------------------------------------------
-- | Alignable functors supporting an \"inverse\" to 'align': splitting
--   a union shape into its component parts.
--
--   Minimal definition: nothing; a default definition is provided,
--   but it may not have the desired definition for all functors. See
--   the source for more information.
--
--   Laws:
--
-- @
-- unalign nil                 = (nil,           nil)
-- unalign (This        \<$> x) = (Just    \<$> x, Nothing \<$  x)
-- unalign (That        \<$> y) = (Nothing \<$  y, Just    \<$> y)
-- unalign (join These  \<$> x) = (Just    \<$> x, Just    \<$> x)
-- unalign ((x \`These`) \<$> y) = (Just x  \<$  y, Just    \<$> y)
-- unalign ((\`These` y) \<$> x) = (Just    \<$> x, Just y  \<$  x)
-- @
class (Align f) => Unalign f where
    -- This might need more laws. Specifically, some notion of not
    -- duplicating the effects would be nice, and a way to express its
    -- relationship with align.
    unalign :: f (These a b) -> (f (Maybe a), f (Maybe b))
    unalign x = (fmap left x, fmap right x)
      where left  = these Just (const Nothing) (\a _ -> Just a)
            right = these (const Nothing) Just (\_ b -> Just b)

instance Unalign Maybe

instance Unalign [] where
    unalign = foldr (these a b ab) ([],[]) 
      where a  l   ~(ls,rs) = (Just l :ls, Nothing:rs)
            b    r ~(ls,rs) = (Nothing:ls, Just r :rs)
            ab l r ~(ls,rs) = (Just l :ls, Just r :rs)

instance Unalign ZipList where
    unalign (ZipList xs) = (ZipList ys, ZipList zs)
      where (ys, zs) = unalign xs

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

instance Crosswalk Identity where
    crosswalk f (Identity a) = fmap Identity (f a)

instance Crosswalk Maybe where
    crosswalk _ Nothing = nil
    crosswalk f (Just a) = Just <$> f a

instance Crosswalk [] where
    crosswalk _ [] = nil
    crosswalk f (x:xs) = alignWith cons (f x) (crosswalk f xs)
      where cons = these pure id (:)

instance Crosswalk (These a) where
    crosswalk _ (This _) = nil
    crosswalk f (That x) = That <$> f x
    crosswalk f (These a x) = These a <$> f x

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

instance Bicrosswalk Either where
    bicrosswalk f _ (Left x)  = Left  <$> f x
    bicrosswalk _ g (Right x) = Right <$> g x

instance Bicrosswalk These where
    bicrosswalk f _ (This x) = This <$> f x
    bicrosswalk _ g (That x) = That <$> g x
    bicrosswalk f g (These x y) = align (f x) (g y)
