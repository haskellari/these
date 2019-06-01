{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- | Module     :  Data.Align
--
-- 'These'-based zipping and unzipping of functors with non-uniform
-- shapes, plus traversal of (bi)foldable (bi)functors through said
-- functors.
module Data.Align (
      Semialign (..)
    , Align(..)
    -- * Specialized aligns
    , malign, salign, Salign (..), padZip, padZipWith
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

import Prelude ()
import Prelude.Compat

import Control.Applicative               (ZipList (..))
import Data.Bifoldable                   (Bifoldable (..))
import Data.Bifunctor                    (Bifunctor (..))
import Data.Functor.Compose              (Compose (..))
import Data.Functor.Identity             (Identity (..))
import Data.Functor.Product              (Product (..))
import Data.Hashable                     (Hashable (..))
import Data.HashMap.Strict               (HashMap)
import Data.List.NonEmpty                (NonEmpty (..))
import Data.Maybe                        (catMaybes)
import Data.Proxy                        (Proxy (..))
import Data.Semigroup                    (Semigroup (..))
import Data.Sequence                     (Seq)
import Data.Tagged                       (Tagged (..))
import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import Data.Vector.Generic               (Vector, empty, stream, unstream)

import qualified Data.HashMap.Strict               as HashMap
import qualified Data.Sequence                     as Seq
import qualified Data.Tree                         as T
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Generic               as VG (foldr, fromList)

#if MIN_VERSION_vector(0,11,0)
import           Data.Vector.Fusion.Bundle.Monadic (Bundle (..))
import qualified Data.Vector.Fusion.Bundle.Monadic as Bundle
import qualified Data.Vector.Fusion.Bundle.Size    as Bundle
#else
import qualified Data.Vector.Fusion.Stream.Size as Stream
#endif

#if MIN_VERSION_containers(0,5,0)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

#if MIN_VERSION_containers(0,5,9)
import qualified Data.IntMap.Merge.Lazy as IntMap
import qualified Data.Map.Merge.Lazy    as Map
#endif

-- containers <0.5
#else
import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
#endif

import Data.These

oops :: String -> a
oops = error . ("Data.Align: internal error: " ++)

-- --------------------------------------------------------------------------
-- | Functors supporting a zip operation that takes the union of
--   non-uniform shapes.
--
-- If your functor is actually a functor from @Kleisli Maybe@ to
-- @Hask@ (so it supports @maybeMap :: (a -> Maybe b) -> f a -> f
-- b@), then an @Align@ instance is making your functor lax monoidal
-- w.r.t. the cartesian monoidal structure on @Kleisli Maybe@,
-- because @These@ is the cartesian product in that category @(a ->
-- Maybe (These b c) ~ (a -> Maybe b, a -> Maybe c))@. This insight
-- is due to rwbarton.
--
-- Minimal definition: either @align@ or @alignWith@.
--
-- == Laws:
--
-- @
-- join align = fmap (join These)
-- align (f \<$> x) (g \<$> y) = bimap f g \<$> align x y
-- alignWith f a b = f \<$> align a b
-- align x (align y z) = fmap assoc (align (align x y) z)
-- @
--
-- /Note:/ @'join' f x = f x x@
--
-- And an addition property if @f@ is 'Foldable',
-- which tries to enforce 'align'-feel:
-- neither values are duplicated nor lost.
--
-- @
-- toList x = toListOf (folded . here) (align x y)
--          = mapMaybe justHere (toList (align x y))
-- @
--
class Functor f => Semialign f where
    -- | Analogous to @'zip'@, combines two structures by taking the union of
    --   their shapes and using @'These'@ to hold the elements.
    align :: f a -> f b -> f (These a b)
    align = alignWith id

    -- | Analogous to @'zipWith'@, combines two structures by taking the union of
    --   their shapes and combining the elements with the given function.
    alignWith :: (These a b -> c) -> f a -> f b -> f c
    alignWith f a b = f <$> align a b

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL align | alignWith #-}
#endif

-- | A unit of 'align'.
--
-- == Laws:
--
-- @
-- (\`align` nil) = fmap This
-- (nil \`align`) = fmap That
-- @
--
class Semialign f => Align f where
    -- | An empty structure. @'align'@ing with @'nil'@ will produce a structure with
    --   the same shape and elements as the other input, modulo @'This'@ or @'That'@.
    nil :: f a

{-# RULES

"align nil nil" align nil nil = nil
"align x x" forall x. align x x = fmap (\y -> These y y) x

"alignWith f nil nil" forall f. alignWith f nil nil = nil
"alignWith f x x" forall f x. alignWith f x x = fmap (\y -> f (These y y)) x

 #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | @since 0.8.1
instance Semialign ((->) e) where
    align f g x = These (f x) (g x)
    alignWith h f g x = h (These (f x) (g x))

instance Align Maybe where
    nil = Nothing

instance Semialign Maybe where
    align Nothing Nothing = Nothing
    align (Just a) Nothing = Just (This a)
    align Nothing (Just b) = Just (That b)
    align (Just a) (Just b) = Just (These a b)

instance Align [] where
    nil = []

instance Semialign [] where
    align xs [] = This <$> xs
    align [] ys = That <$> ys
    align (x:xs) (y:ys) = These x y : align xs ys

-- @since 0.8
instance Semialign NonEmpty where
    align (x :| xs) (y :| ys) = These x y :| align xs ys

instance Align ZipList where
    nil = ZipList []

instance Semialign ZipList where
    alignWith f (ZipList xs) (ZipList ys) = ZipList (alignWith f xs ys)

instance Align Seq where
    nil = Seq.empty

instance Semialign Seq where
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

-- | @since 0.8.1
instance Semialign T.Tree where
    align (T.Node x xs) (T.Node y ys) = T.Node (These x y) (alignWith (these (fmap This) (fmap That) align) xs ys)

instance (Ord k) => Align (Map k) where
    nil = Map.empty

instance (Ord k) => Semialign (Map k) where
#if MIN_VERSION_containers(0,5,9)
    alignWith f = Map.merge (Map.mapMissing (\_ x ->  f (This x)))
                            (Map.mapMissing (\_ y ->  f (That y)))
                            (Map.zipWithMatched (\_ x y -> f (These x y)))
#elif MIN_VERSION_containers(0,5,0)
    alignWith f = Map.mergeWithKey (\_ x y -> Just $ f $ These x y) (fmap (f . This)) (fmap (f . That))
#else
    align m n = Map.unionWith merge (Map.map This m) (Map.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align Map: merge"
#endif

instance Align IntMap where
    nil = IntMap.empty

instance Semialign IntMap where
#if MIN_VERSION_containers(0,5,9)
    alignWith f = IntMap.merge (IntMap.mapMissing (\_ x ->  f (This x)))
                               (IntMap.mapMissing (\_ y ->  f (That y)))
                               (IntMap.zipWithMatched (\_ x y -> f (These x y)))
#elif MIN_VERSION_containers(0,5,0)
    alignWith f = IntMap.mergeWithKey (\_ x y -> Just $ f $ These x y) (fmap (f . This)) (fmap (f . That))
#else
    align m n = IntMap.unionWith merge (IntMap.map This m) (IntMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align IntMap: merge"
#endif

-- @since 0.8
instance Semialign Identity where
    alignWith f (Identity a) (Identity b) = Identity (f (These a b))

instance (Align f, Align g) => Align (Product f g) where
    nil = Pair nil nil

instance (Semialign f, Semialign g) => Semialign (Product f g) where
    align (Pair a b) (Pair c d) = Pair (align a c) (align b d)
    alignWith f (Pair a b) (Pair c d) = Pair (alignWith f a c) (alignWith f b d)

-- | @since 0.8.1
instance (Align f, Semialign g) => Align (Compose f g) where
    nil = Compose nil

-- | @since 0.8.1
instance (Semialign f, Semialign g) => Semialign (Compose f g) where
    align (Compose x) (Compose y) = Compose (alignWith (these (fmap This) (fmap That) align) x y)

-- Based on the Data.Vector.Fusion.Stream.Monadic zipWith implementation
instance Monad m => Align (Stream m) where
    nil = Stream.empty

instance Monad m => Semialign (Stream m) where
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

instance Monad m => Semialign (Bundle m v) where
    alignWith f Bundle{sElems = sa, sSize = na} Bundle{sElems = sb, sSize = nb}
      = Bundle.fromStream (alignWith f sa sb) (Bundle.larger na nb)
#endif

instance Align V.Vector where
  nil = Data.Vector.Generic.empty

instance Semialign V.Vector where
  alignWith = alignVectorWith

alignVectorWith :: (Vector v a, Vector v b, Vector v c)
        => (These a b -> c) -> v a -> v b -> v c
alignVectorWith f x y = unstream $ alignWith f (stream x) (stream y)

instance (Eq k, Hashable k) => Align (HashMap k) where
    nil = HashMap.empty

instance (Eq k, Hashable k) => Semialign (HashMap k) where
    align m n = HashMap.unionWith merge (HashMap.map This m) (HashMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align HashMap: merge"

-- | @since 0.8.1
instance Semialign (Tagged b) where
    alignWith f (Tagged x) (Tagged y) = Tagged (f (These x y))

-- | @since 0.8.1
instance Semialign Proxy where
    alignWith _ _ _ = Proxy
    align _ _       = Proxy

-- | @since 0.8.1
instance Align Proxy where
    nil = Proxy

-- | Align two structures and combine with 'mappend'.
--
-- See `salign`. `malign` will be deprecated after `Semigroup` becomes a super
-- class of `Monoid`
malign :: (Semialign f, Monoid a) => f a -> f a -> f a
malign = alignWith (mergeThese mappend)

-- | Align two structures and combine with '<>'.
--
-- @since 0.7.3
salign :: (Semialign f, Semigroup a) => f a -> f a -> f a
salign = alignWith (mergeThese (<>))

-- | Monoid under 'salign' and 'nil'.
newtype Salign f a = Salign (f a)

instance (Align f, Semigroup a) => Semigroup (Salign f a) where
    Salign x <> Salign y = Salign (salign x y)

instance (Align f, Semigroup a) => Monoid (Salign f a) where
    mappend = (<>)
    mempty = Salign nil

-- | Align two structures as in 'zip', but filling in blanks with 'Nothing'.
padZip :: (Semialign f) => f a -> f b -> f (Maybe a, Maybe b)
padZip = alignWith (fromThese Nothing Nothing . bimap Just Just)

-- | Align two structures as in 'zipWith', but filling in blanks with 'Nothing'.
padZipWith :: (Semialign f) => (Maybe a -> Maybe b -> c) -> f a -> f b -> f c
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
