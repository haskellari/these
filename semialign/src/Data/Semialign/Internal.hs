{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
module Data.Semialign.Internal where

import Prelude ()
import Prelude.Compat hiding (repeat, unzip, zip, zipWith)

import qualified Prelude.Compat as Prelude

import Control.Applicative               (ZipList (..))
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

import qualified Data.HashMap.Strict               as HM
import qualified Data.List.NonEmpty                as NE
import qualified Data.Sequence                     as Seq
import qualified Data.Tree                         as T
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as Stream

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
import Data.These.Combinators

oops :: String -> a
oops = error . ("Data.Align: internal error: " ++)

-- --------------------------------------------------------------------------
-- | Functors supporting an 'align' operation that takes the union of
-- non-uniform shapes.
--
-- Minimal definition: either 'align' or 'alignWith'.
--
-- == Laws
--
-- The laws of 'align' and 'zip' resemble lattice laws.
-- There is a plenty of laws, but they are simply satisfied.
--
-- And an addition property if @f@ is 'Foldable',
-- which tries to enforce 'align'-feel:
-- neither values are duplicated nor lost.
--
--
-- /Note:/ @'join' f x = f x x@
--
-- /Idempotency/
--
-- @
-- join align ≡ fmap (join These)
-- @
--
-- /Commutativity/
--
-- @
-- align x y ≡ swap \<$> align y x
-- @
--
-- /Associativity/
--
-- @
-- align x (align y z) ≡ assoc \<$> align (align x y) z
-- @
--
-- /With/
--
-- @
-- alignWith f a b ≡ f \<$> align a b
-- @
--
-- /Functoriality/
--
-- @
-- align (f \<$> x) (g \<$> y) ≡ bimap f g \<$> align x y
-- @
--
-- /Alignedness/, if @f@ is 'Foldable'
--
-- @
-- toList x ≡ toListOf (folded . here) (align x y)
--          ≡ mapMaybe justHere (toList (align x y))
-- @
--
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
    {-# MINIMAL (align | alignWith) #-}
#endif

-- | A unit of 'align'.
--
-- == Laws
--
-- @
-- (\`align` nil) ≡ fmap This
-- (nil \`align`) ≡ fmap That
-- @
--
class Semialign f => Align f where
    -- | An empty structure. @'align'@ing with @'nil'@ will produce a structure with
    --   the same shape and elements as the other input, modulo @'This'@ or @'That'@.
    nil :: f a

-- |
--
-- Alignable functors supporting an \"inverse\" to 'align': splitting
-- a union shape into its component parts.
--
-- == Laws
--
-- @
-- uncurry align (unalign xs) ≡ xs
-- unalign (align xs ys) ≡ (xs, ys)
-- @
--
-- == Compatibility note
--
-- In version 1 'unalign' was changed to return @(f a, f b)@ pair,
-- instead of @(f (Just a), f (Just b))@. Old behaviour can be achieved with
-- if ever needed.
--
-- >>> unzipWith (unalign . Just) [This 'a', That 'b', These 'c' 'd']
-- ([Just 'a',Nothing,Just 'c'],[Nothing,Just 'b',Just 'd'])
--
class Semialign f => Unalign f where
    unalign :: f (These a b) -> (f a, f b)
    unalign = unalignWith id

    unalignWith :: (c -> These a b) -> f c -> (f a, f b)
    unalignWith f fx = unalign (fmap f fx)

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL unalignWith | unalign #-}
#endif


-- | Functors supporting a 'zip' operation that takes the intersection of
-- non-uniform shapes.
--
-- Minimal definition: either 'zip' or 'zipWith'.
--
-- /Idempotency/
--
-- @
-- join zip   ≡ fmap (join (,))
-- @
--
-- /Commutativity/
--
-- @
-- zip x y ≡ swap \<$> zip y x
-- @
--
-- /Associativity/
--
-- @
-- zip x (zip y z) ≡ assoc \<$> zip (zip x y) z
-- @
--
-- /Absorption/
--
-- @
-- fst    \<$> zip xs (align xs ys) ≡ xs
-- toThis \<$> align xs (zip xs ys) ≡ This \<$> xs
--   where
--     toThis (This a)    = This a
--     toThis (These a _) = This a
--     toThis (That b)    = That b
-- @
--
-- /With/
--
-- @
-- zipWith f a b ≡ f \<$> zip a b
-- @
--
-- /Functoriality/
--
-- @
-- zip (f \<$> x) (g \<$> y) ≡ bimap f g \<$> zip x y
-- @
--
-- /Zippyness/
--
-- @
-- fmap fst (zip x x) ≡ x
-- fmap snd (zip x x) ≡ x
-- zip (fmap fst x) (fmap snd x) ≡ x
-- @
--
-- /Distributivity/
--
-- @
--                    align (zip xs ys) zs ≡ undistrThesePair \<$> zip (align xs zs) (align ys zs)
-- distrPairThese \<$> zip (align xs ys) zs ≡                      align (zip xs zs) (zip ys zs)
--                    zip (align xs ys) zs ≡ undistrPairThese \<$> align (zip xs zs) (zip ys zs)
-- @
--
-- /Note/, the following doesn't hold:
--
-- @
-- distrThesePair \<$> align (zip xs ys) zs ≢ zip (align xs zs) (align ys zs)
-- @
--
-- when @xs = []@ and @ys = zs = [0]@, then
-- the left hand side is "only" @[('That' 0, 'That' 0)]@,
-- but the right hand side is @[('That' 0, 'These' 0 0)]@.
--
class Semialign f => Zip f where
    -- | Combines to structures by taking the intersection of their shapes
    -- and using pair to hold the elements.
    zip :: f a -> f b -> f (a, b)
    zip = zipWith (,)
    --
    -- | Combines to structures by taking the intersection of their shapes
    -- and combining the elements with the given function.
    zipWith :: (a -> b -> c) -> f a -> f b -> f c
    zipWith f a b = uncurry f <$> zip a b

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (zip | zipWith) #-}
#endif

-- | Zippable functors supporting left and right units
--
-- /Unit/
--
-- @
-- fst \<$> zip xs (repeat y) ≡ xs
-- snd \<$> zip (repeat x) ys ≡ ys
-- @
--
class Zip f => Repeat f where
    -- | A /repeat/ structure.
    repeat :: a -> f a

-- | Right inverse of 'zip'.
--
-- This class is definable for every 'Functor'. See 'unzipDefault'.
--
-- == Laws
--
-- @
-- uncurry zip (unzip xs) ≡ xs
-- unzip (zip xs xs) ≡ (xs, xs)
-- @
--
-- Note:
--
-- @
-- unzip (zip xs ys) ≢ (xs, _) or (_, ys)
-- @
--
-- For sequence-like types this holds, but for Map-like it doesn't.
--
class Zip f => Unzip f where
    unzipWith :: (c -> (a, b)) -> f c -> (f a, f b)
    unzipWith f = unzip . fmap f

    unzip :: f (a, b) -> (f a, f b)
    unzip = unzipWith id

#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL unzipWith | unzip #-}
#endif

unzipDefault :: Functor f => f (a, b) -> (f a, f b)
unzipDefault x = (fst <$> x, snd <$> x)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Semialign ((->) e) where
    align f g x = These (f x) (g x)
    alignWith h f g x = h (These (f x) (g x))

instance Zip ((->) e) where
    zip f g x = (f x, g x)

instance Repeat ((->) e) where
    repeat = pure

instance Semialign Maybe where
    align Nothing Nothing = Nothing
    align (Just a) Nothing = Just (This a)
    align Nothing (Just b) = Just (That b)
    align (Just a) (Just b) = Just (These a b)

instance Zip Maybe where
    zip Nothing  _        = Nothing
    zip (Just _) Nothing  = Nothing
    zip (Just a) (Just b) = Just (a, b)

instance Repeat Maybe where
    repeat = Just

instance Unalign Maybe where
    unalign Nothing            = (Nothing, Nothing)
    unalign (Just (This a))    = (Just a, Nothing)
    unalign (Just (That b))    = (Nothing, Just b)
    unalign (Just (These a b)) = (Just a, Just b)

instance Unzip Maybe where
    unzip = unzipDefault

instance Align Maybe where
    nil = Nothing


instance Semialign [] where
    align xs [] = This <$> xs
    align [] ys = That <$> ys
    align (x:xs) (y:ys) = These x y : align xs ys

instance Align [] where
    nil = []

instance Zip [] where
    zip     = Prelude.zip
    zipWith = Prelude.zipWith

instance Repeat [] where
    repeat = Prelude.repeat

instance Unzip [] where
    unzip = Prelude.unzip


-- | @'zipWith' = 'liftA2'@ .
instance Semialign ZipList where
    alignWith f (ZipList xs) (ZipList ys) = ZipList (alignWith f xs ys)

instance Align ZipList where
    nil = ZipList []

instance Zip ZipList where
    zipWith   f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

instance Repeat ZipList where
    repeat = pure

instance Unzip ZipList where
    unzip (ZipList xs) = (ZipList ys, ZipList zs) where
        (ys, zs) = unzip xs

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

instance Semialign NonEmpty where
    align (x :| xs) (y :| ys) = These x y :| align xs ys

instance Zip NonEmpty where
    zip     = NE.zip
    zipWith = NE.zipWith

instance Repeat NonEmpty where
    repeat = NE.repeat

instance Unzip NonEmpty where
    unzip = NE.unzip

-------------------------------------------------------------------------------
-- containers: ListLike
-------------------------------------------------------------------------------

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

instance Align Seq where
    nil = Seq.empty

instance Unzip Seq where
#if MIN_VERSION_containers(0,5,11)
    unzip     = Seq.unzip
    unzipWith = Seq.unzipWith
#else
    unzip = unzipDefault
#endif

instance Zip Seq where
    zip     = Seq.zip
    zipWith = Seq.zipWith

instance Semialign T.Tree where
    align (T.Node x xs) (T.Node y ys) = T.Node (These x y) (alignWith (these (fmap This) (fmap That) align) xs ys)

instance Zip T.Tree where
    zipWith f (T.Node x xs) (T.Node y ys) = T.Node (f x y) (zipWith (zipWith f) xs ys)

instance Repeat T.Tree where
    repeat x = n where n = T.Node x (repeat n)

instance Unzip T.Tree where
    unzipWith f = go where
        go  (T.Node x xs) = (T.Node y ys, T.Node z zs) where
            ~(y, z) = f x
            ~(ys, zs) = unzipWith go xs

-------------------------------------------------------------------------------
-- containers: MapLike
-------------------------------------------------------------------------------

instance Ord k => Semialign (Map k) where
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

instance (Ord k) => Align (Map k) where
    nil = Map.empty

instance Ord k => Unalign (Map k) where
    unalign xs = (Map.mapMaybe justHere xs, Map.mapMaybe justThere xs)

instance Ord k => Unzip (Map k) where unzip = unzipDefault

instance Ord k => Zip (Map k) where
    zipWith = Map.intersectionWith

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

instance Align IntMap where
    nil = IntMap.empty

instance Unalign IntMap where
    unalign xs = (IntMap.mapMaybe justHere xs, IntMap.mapMaybe justThere xs)

instance Unzip IntMap where unzip = unzipDefault

instance Zip IntMap where
    zipWith = IntMap.intersectionWith

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance Semialign Identity where
    alignWith f (Identity a) (Identity b) = Identity (f (These a b))

instance Zip Identity where
    zipWith f (Identity a) (Identity b) = Identity (f a b)

instance Repeat Identity where
    repeat = pure

instance Unzip Identity where
    unzip (Identity ~(a, b)) = (Identity a, Identity b)


instance (Semialign f, Semialign g) => Semialign (Product f g) where
    align (Pair a b) (Pair c d) = Pair (align a c) (align b d)
    alignWith f (Pair a b) (Pair c d) = Pair (alignWith f a c) (alignWith f b d)

instance (Unalign f, Unalign g) => Unalign (Product f g) where
    unalign (Pair a b) = (Pair al bl, Pair ar br) where
        ~(al, ar) = unalign a
        ~(bl, br) = unalign b

instance (Align f, Align g) => Align (Product f g) where
    nil = Pair nil nil

instance (Zip f, Zip g) => Zip (Product f g) where
    zip (Pair a b) (Pair c d) = Pair (zip a c) (zip b d)
    zipWith f (Pair a b) (Pair c d) = Pair (zipWith f a c) (zipWith f b d)

instance (Repeat f, Repeat g) => Repeat (Product f g) where
    repeat x = Pair (repeat x) (repeat x)

instance (Unzip f, Unzip g) => Unzip (Product f g) where
    unzip (Pair a b) = (Pair al bl, Pair ar br) where
        ~(al, ar) = unzip a
        ~(bl, br) = unzip b


instance (Semialign f, Semialign g) => Semialign (Compose f g) where
    alignWith f (Compose x) (Compose y) = Compose (alignWith g x y) where
        g (This ga)     = fmap (f . This) ga
        g (That gb)     = fmap (f . That) gb
        g (These ga gb) = alignWith f ga gb

instance (Align f, Semialign g) => Align (Compose f g) where
    nil = Compose nil

instance (Zip f, Zip g) => Zip (Compose f g) where
    zipWith f (Compose x) (Compose y) = Compose (zipWith (zipWith f) x y)

instance (Repeat f, Repeat g) => Repeat (Compose f g) where
    repeat x = Compose (repeat (repeat x))

instance (Unzip f, Unzip g) => Unzip (Compose f g) where
    unzipWith f (Compose x) = (Compose y, Compose z) where
        ~(y, z) = unzipWith (unzipWith f) x

-- This is unlawful instance.
--
-- instance (Unalign f, Unalign g) => Unalign (Compose f g) where
--     unalignWith f (Compose x) = (Compose y, Compose z) where
--         ~(y, z) = unalignWith (uncurry These . unalignWith f) x

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

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

instance Monad m => Zip (Stream m) where
    zipWith = Stream.zipWith

#if MIN_VERSION_vector(0,11,0)
instance Monad m => Align (Bundle m v) where
    nil = Bundle.empty

instance Monad m => Semialign (Bundle m v) where
    alignWith f Bundle{sElems = sa, sSize = na} Bundle{sElems = sb, sSize = nb}
      = Bundle.fromStream (alignWith f sa sb) (Bundle.larger na nb)
#endif

instance Monad m => Zip (Bundle m v) where
    zipWith = Bundle.zipWith

instance Semialign V.Vector where
    alignWith = alignVectorWith

instance Zip V.Vector where
    zipWith = V.zipWith

instance Align V.Vector where
    nil = Data.Vector.Generic.empty

instance Unzip V.Vector where
    unzip = V.unzip

alignVectorWith :: (Vector v a, Vector v b, Vector v c)
        => (These a b -> c) -> v a -> v b -> v c
alignVectorWith f x y = unstream $ alignWith f (stream x) (stream y)

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (Eq k, Hashable k) => Align (HashMap k) where
    nil = HM.empty

instance (Eq k, Hashable k) => Semialign (HashMap k) where
    align m n = HM.unionWith merge (HM.map This m) (HM.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = oops "Align HashMap: merge"

instance (Eq k, Hashable k) => Zip (HashMap k) where
    zipWith = HM.intersectionWith

instance (Eq k, Hashable k) => Unzip   (HashMap k) where unzip = unzipDefault

instance (Eq k, Hashable k) => Unalign (HashMap k) where
    unalign xs = (HM.mapMaybe justHere xs, HM.mapMaybe justThere xs)

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance Semialign (Tagged b) where
    alignWith f (Tagged x) (Tagged y) = Tagged (f (These x y))

instance Zip (Tagged b) where
    zipWith f (Tagged x) (Tagged y) = Tagged (f x y)

instance Repeat (Tagged b) where
    repeat = Tagged

instance Unzip (Tagged b) where
    unzip (Tagged ~(a, b)) = (Tagged a, Tagged b)


instance Semialign Proxy where
    alignWith _ _ _ = Proxy
    align _ _       = Proxy

instance Align Proxy where
    nil = Proxy

instance Unalign Proxy where
    unalign _ = (Proxy, Proxy)

instance Zip Proxy where
    zipWith _ _ _ = Proxy
    zip _ _       = Proxy

instance Repeat Proxy where
    repeat _ = Proxy

instance Unzip Proxy where
    unzip _ = (Proxy, Proxy)

-------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------

-- | Align two structures and combine with '<>'.
salign :: (Semialign f, Semigroup a) => f a -> f a -> f a
salign = alignWith (mergeThese (<>))

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
