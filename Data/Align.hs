{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- | Module     :  Data.Align
--
-- 'These'-based zipping and unzipping of functors with non-uniform
-- shapes, plus traversal of (bi)foldable (bi)functors through said
-- functors.
module Data.Align (
                    Partitionable(..)
                  , mapPartitionable
                  , Align(..)
                  -- * Specialized aligns
                  , malign, padZip, padZipWith
                  , lpadZip, lpadZipWith
                  , rpadZip, rpadZipWith
                  , alignVectorWith

                  -- * Unalign
                  , Unalign(..)

                  -- * Crosswalk
                  , Crosswalk(..)

                  -- * Bicrosswalk
                  , Bicrosswalk(..)
                  ) where

-- TODO: More instances..

import Control.Applicative
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Product
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes)
import Data.Monoid hiding (Product)
import Data.Sequence (Seq)
import Data.These
import qualified Data.Vector as V
import Data.Vector.Generic (Vector, unstream, stream, empty)
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Generic as VG (fromList, foldr)

#if MIN_VERSION_vector(0,11,0)
import Data.Vector.Fusion.Bundle.Monadic (Bundle (..))
import qualified Data.Vector.Fusion.Bundle.Monadic as Bundle
import qualified Data.Vector.Fusion.Bundle.Size as Bundle
#else
import qualified Data.Vector.Fusion.Stream.Size as Stream
#endif

#if MIN_VERSION_containers(0, 5, 0)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
#else
import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
#endif

import Prelude hiding (foldr) -- Fix redundant import warnings

oops :: String -> a
oops = error . ("Data.Align: internal error: " ++)

-- --------------------------------------------------------------------------
-- | Functors supporting a partition operation that breaks the shape in two.
--
--   In terms of category theory,
--   a partitionable functor is a functor from @Kleisli Maybe@ to @Hask@.
--
--   @'partitionMap'@ is similar to @'unzip'@.
--   It maps each position in the input to one of the ouputs.
--   Contrastly, @'cover'@ maps each position to one or both of the outputs.
--   @'fmapMaybe'@ maps only some positions to the output.
class Functor f => Partitionable f where
    -- | Break the structure in two.
    --   The outputs may have overlapping shape,
    --   but not necessarily the same shape.
    --
    --   This is akin to a topological <https://en.wikipedia.org/wiki/Cover_(topology) cover>.
    cover :: (a -> These b c) -> f a -> (f b, f c)
    cover f as = ( fmapMaybe (these Just nothing (\a _ -> Just a) . f) as
                 , fmapMaybe (these nothing Just (\_ b -> Just b) . f) as)
      where
        nothing _ = Nothing

    -- | Break the structure into two partitions.
    --   The outputs should have disjoint shape,
    --   as determined by which positions returned @'Left'@ vs @'Right'@.
    partitionMap :: (a -> Either b c) -> f a -> (f b, f c)
    partitionMap f = cover (either This That . f)

    -- | Remove positions from the structure.
    --   Positions which return @'Nothing'@ will be removed.
    fmapMaybe :: (a -> Maybe b) -> f a -> f b
    fmapMaybe f = fst . partitionMap (maybe (Right ()) Left . f)
#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (cover | partitionMap | fmapMaybe) #-}
#endif

-- | A function suitable for use as the implementation of @'fmap'@.
mapPartitionable :: Partitionable f => (a -> b) -> f a -> f b
mapPartitionable f = fmapMaybe (Just . f)

instance Partitionable Maybe where
    cover _ Nothing = (Nothing, Nothing)
    cover f (Just a) = case f a of
        This b -> (Just b, Nothing)
        That c -> (Nothing, Just c)
        These b c -> (Just b, Just c)

instance Partitionable [] where
    cover f = foldr consThese ([], [])
      where
        consThese x (as, bs) = case f x of
            This a -> (a : as, bs)
            That b -> (as, b : bs)
            These a b -> (a : as, b : bs)

instance Partitionable ZipList where
    cover f (ZipList as) = let
        (bs, cs) = cover f as
        in (ZipList bs, ZipList cs)
    partitionMap f (ZipList as) = let
        (bs, cs) = partitionMap f as
        in (ZipList bs, ZipList cs)
    fmapMaybe f (ZipList as) = ZipList (fmapMaybe f as)

instance Partitionable Seq where
    cover f = foldr consThese (Seq.empty, Seq.empty)
      where
        consThese x (as, bs) = case f x of
            This a -> (a Seq.<| as, bs)
            That b -> (as, b Seq.<| bs)
            These a b -> (a Seq.<| as, b Seq.<| bs)

instance (Ord k) => Partitionable (Map k) where
    cover f = Map.foldrWithKey consThese (Map.empty, Map.empty)
      where
        consThese k x (as, bs) = case f x of
            This a -> (Map.insert k a as, bs)
            That b -> (as, Map.insert k b bs)
            These a b -> (Map.insert k a as, Map.insert k b bs)

instance Partitionable IntMap where
    cover f = IntMap.foldrWithKey consThese (IntMap.empty, IntMap.empty)
      where
        consThese k x (as, bs) = case f x of
            This a -> (IntMap.insert k a as, bs)
            That b -> (as, IntMap.insert k b bs)
            These a b -> (IntMap.insert k a as, IntMap.insert k b bs)

instance (Partitionable f, Partitionable g) => Partitionable (Product f g) where
    cover f (Pair fs gs) = let
        (afs, bfs) = cover f fs
        (ags, bgs) = cover f gs
        in (Pair afs ags, Pair bfs bgs)
  
instance Monad m => Partitionable (Stream m) where
    -- Help?

instance Monad m => Partitionable (Bundle m v) where
    -- Help?

instance Partitionable V.Vector where
    cover f = foldr consThese (V.empty, V.empty)
      where
        consThese x (as, bs) = case f x of
            This a -> (a `V.cons` as, bs)
            That b -> (as, b `V.cons` bs)
            These a b -> (a `V.cons` as, b `V.cons` bs)

instance (Eq k, Hashable k) => Partitionable (HashMap k) where
    cover f = HashMap.foldrWithKey consThese (HashMap.empty, HashMap.empty)
      where
        consThese k x (as, bs) = case f x of
            This a -> (HashMap.insert k a as, bs)
            That b -> (as, HashMap.insert k b bs)
            These a b -> (HashMap.insert k a as, HashMap.insert k b bs)

-- --------------------------------------------------------------------------
-- | Functors supporting a zip operation that takes the union of
--   non-uniform shapes.
--
--   If your functor is @'Partitionable'@,
--   then an @Align@ instance is making your functor lax monoidal
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
class (Partitionable f) => Align f where
    -- | An empty strucutre. @'align'@ing with @'nil'@ will produce a structure with
    --   the same shape and elements as the other input, modulo @'This'@ or @'That'@.
    nil :: f a

    -- | Analogous to @'zip'@, combines two structures by taking the union of
    --   their shapes and using @'These'@ to hold the elements.
    align :: f a -> f b -> f (These a b)
    align = alignWith id

    -- | Analogous to @'zipWith'@, combines two structures by taking the union of
    --   their shapes and combining the elements with the given function.
    alignWith :: (These a b -> c) -> f a -> f b -> f c
    alignWith f a b = f <$> align a b

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL nil , (align | alignWith) #-}
#endif

{-# RULES

"align nil nil" align nil nil = nil
"align x x" forall x. align x x = fmap (\y -> These y y) x

"alignWith f nil nil" forall f. alignWith f nil nil = nil
"alignWith f x x" forall f x. alignWith f x x = fmap (\y -> f (These y y)) x

  #-}


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

instance Align Seq where
    nil = Seq.empty

    align xs ys = case compare xn yn of
        EQ -> Seq.zipWith fc xs ys
        LT -> case Seq.splitAt xn ys of
            (ysl, ysr) -> Seq.zipWith These xs ysl `mappend` fmap That ysr
        GT -> case Seq.splitAt yn xs of
            (xsl, xsr) -> Seq.zipWith These xsl ys `mappend` fmap This xsr
      where
        xn = Seq.length xs
        yn = Seq.length ys
        fc = These

    alignWith f xs ys = case compare xn yn of
        EQ -> Seq.zipWith fc xs ys
        LT -> case Seq.splitAt xn ys of
            (ysl, ysr) -> Seq.zipWith fc xs ysl `mappend` fmap (f . That) ysr
        GT -> case Seq.splitAt yn xs of
            (xsl, xsr) -> Seq.zipWith fc xsl ys `mappend` fmap (f . This) xsr
      where
        xn = Seq.length xs
        yn = Seq.length ys
        fc x y = f (These x y)

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

instance (Align f, Align g) => Align (Product f g) where
    nil = Pair nil nil
    align (Pair a b) (Pair c d) = Pair (align a c) (align b d)

-- Based on the Data.Vector.Fusion.Stream.Monadic zipWith implementation
instance Monad m => Align (Stream m) where
    nil = Stream.empty
#if MIN_VERSION_vector(0,11,0)
    alignWith  f (Stream stepa ta) (Stream stepb tb)
      = Stream step (ta, tb, Nothing, False)
#else
    alignWith  f (Stream stepa ta na) (Stream stepb tb nb)
      = Stream step (ta, tb, Nothing, False) (Stream.larger na nb)
#endif
      where
        step (sa, sb, Nothing, False) = do
            r <- stepa sa
            return $ case r of
                Yield x sa' -> Skip (sa', sb, Just x, False)
                Skip    sa' -> Skip (sa', sb, Nothing, False)
                Done        -> Skip (sa, sb, Nothing, True)

        step (sa, sb, av, adone) = do
            r <- stepb sb
            return $ case r of
                Yield y sb' -> Yield (f $ maybe (That y) (`These` y) av)
                                     (sa, sb', Nothing, adone)
                Skip sb'    -> Skip (sa, sb', av, adone)
                Done -> case (av, adone) of
                    (Just x, False) -> Yield (f $ This x) (sa, sb, Nothing, adone)
                    (_, True)       -> Done
                    _               -> Skip (sa, sb, Nothing, False)

#if MIN_VERSION_vector(0,11,0)
instance Monad m => Align (Bundle m v) where
    nil = Bundle.empty
    alignWith f Bundle{sElems = sa, sSize = na} Bundle{sElems = sb, sSize = nb}
      = Bundle.fromStream (alignWith f sa sb) (Bundle.larger na nb)
#endif

instance Align V.Vector where
  nil = Data.Vector.Generic.empty
  alignWith = alignVectorWith

alignVectorWith :: (Vector v a, Vector v b, Vector v c)
        => (These a b -> c) -> v a -> v b -> v c
alignVectorWith f x y = unstream $ alignWith f (stream x) (stream y)

instance (Eq k, Hashable k) => Align (HashMap k) where
    nil = HashMap.empty
    align m n = HashMap.unionWith merge (HashMap.map This m) (HashMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align HashMap: merge"

-- | Align two structures and combine with 'mappend'.
malign :: (Align f, Monoid a) => f a -> f a -> f a
malign = alignWith (mergeThese mappend)

-- | Align two structures as in 'zip', but filling in blanks with 'Nothing'.
padZip :: (Align f) => f a -> f b -> f (Maybe a, Maybe b)
padZip = alignWith (fromThese Nothing Nothing . bimap Just Just)

-- | Align two structures as in 'zipWith', but filling in blanks with 'Nothing'.
padZipWith :: (Align f) => (Maybe a -> Maybe b -> c) -> f a -> f b -> f c
padZipWith f xs ys = uncurry f <$> padZip xs ys

-- | Left-padded 'zipWith'.
lpadZipWith :: (Maybe a -> b -> c) -> [a] -> [b] -> [c]
lpadZipWith f xs ys = catMaybes $ padZipWith (\x y -> f x <$> y) xs ys

-- | Left-padded 'zip'.
lpadZip :: [a] -> [b] -> [(Maybe a, b)]
lpadZip = lpadZipWith (,)

-- | Right-padded 'zipWith'.
rpadZipWith :: (a -> Maybe b -> c) -> [a] -> [b] -> [c]
rpadZipWith f xs ys = lpadZipWith (flip f) ys xs

-- | Right-padded 'zip'.
rpadZip :: [a] -> [b] -> [(a, Maybe b)]
rpadZip = rpadZipWith (,)


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

instance (Unalign f, Unalign g) => Unalign (Product f g) where
    unalign (Pair a b) = (Pair al bl, Pair ar br)
      where (al, ar) = unalign a
            (bl, br) = unalign b

instance Monad m => Unalign (Stream m)

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
