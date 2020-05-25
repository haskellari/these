{-# LANGUAGE CPP                #-}
-- | The 'These' type and associated operations. Now enhanced with "Control.Lens" magic!
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}
module Data.These (
      These(..)

    -- * Functions to get rid of 'These'
    , these
    , fromThese
    , mergeThese
    , mergeTheseWith

    -- * Partition
    , partitionThese
    , partitionHereThere
    , partitionEithersNE

    -- * Distributivity
    --
    -- | This distributivity combinators aren't isomorphisms!
    , distrThesePair
    , undistrThesePair
    , distrPairThese
    , undistrPairThese
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq    (NFData (..))
import Data.Bifoldable    (Bifoldable (..))
import Data.Bifunctor     (Bifunctor (..))
import Data.Binary        (Binary (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Data          (Data, Typeable)
import Data.Either        (partitionEithers)
import Data.Hashable      (Hashable (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup     (Semigroup (..))
import GHC.Generics       (Generic)

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif

#ifdef MIN_VERSION_assoc
import Data.Bifunctor.Assoc (Assoc (..))
import Data.Bifunctor.Swap  (Swap (..))
#endif

-- $setup
-- >>> import Control.Lens

-- --------------------------------------------------------------------------
-- | The 'These' type represents values with two non-exclusive possibilities.
--
--   This can be useful to represent combinations of two values, where the
--   combination is defined if either input is. Algebraically, the type
--   @'These' A B@ represents @(A + B + AB)@, which doesn't factor easily into
--   sums and products--a type like @'Either' A (B, 'Maybe' A)@ is unclear and
--   awkward to use.
--
--   'These' has straightforward instances of 'Functor', 'Monad', &c., and
--   behaves like a hybrid error/writer monad, as would be expected.
--
--   For zipping and unzipping of structures with 'These' values, see
--   "Data.Align".
data These a b = This a | That b | These a b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
    )

-------------------------------------------------------------------------------
-- Eliminators
-------------------------------------------------------------------------------

-- | Case analysis for the 'These' type.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these l _ _ (This a) = l a
these _ r _ (That x) = r x
these _ _ lr (These a x) = lr a x

-- | Takes two default values and produces a tuple.
fromThese :: a -> b -> These a b -> (a, b)
fromThese x y = these (`pair` y) (x `pair`) pair where
    pair = (,)

-- | Coalesce with the provided operation.
mergeThese :: (a -> a -> a) -> These a a -> a
mergeThese = these id id

-- | 'bimap' and coalesce results with the provided operation.
mergeTheseWith :: (a -> c) -> (b -> c) -> (c -> c -> c) -> These a b -> c
mergeTheseWith f g op t = mergeThese op $ bimap f g t

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

-- | Select each constructor and partition them into separate lists.
partitionThese :: [These a b] -> ([a], [b], [(a, b)])
partitionThese []     = ([], [], [])
partitionThese (t:ts) = case t of
    This x    -> (x : xs,     ys,         xys)
    That y    -> (    xs, y : ys,         xys)
    These x y -> (    xs,     ys, (x,y) : xys)
  where
    ~(xs,ys,xys) = partitionThese ts

-- | Select 'here' and 'there' elements and partition them into separate lists.
--
-- @since 0.8
partitionHereThere :: [These a b] -> ([a], [b])
partitionHereThere []     = ([], [])
partitionHereThere (t:ts) = case t of
    This x     -> (x : xs,     ys)
    That y     -> (    xs, y : ys)
    These x  y -> (x : xs, y : ys)
  where
    ~(xs,ys) = partitionHereThere ts

-- | Like 'partitionEithers' but for 'NonEmpty' types.
--
-- * either all are 'Left'
-- * either all are 'Right'
-- * or there is both 'Left' and 'Right' stuff
--
-- /Note:/ this is not online algorithm. In the worst case it will traverse
-- the whole list before deciding the result constructor.
--
-- >>> partitionEithersNE $ Left 'x' :| [Right 'y']
-- These ('x' :| "") ('y' :| "")
--
-- >>> partitionEithersNE $ Left 'x' :| map Left "yz"
-- This ('x' :| "yz")
--
-- @since 1.0.1
partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
    (Left y,  ys,   [])   -> This (y :| ys)
    (Left y,  ys,   z:zs) -> These (y :| ys) (z :| zs)
    (Right z, [],   zs)   -> That (z :| zs)
    (Right z, y:ys, zs)   -> These (y :| ys) (z :| zs)
  where
    (ls, rs) = partitionEithers xs


-------------------------------------------------------------------------------
-- Distributivity
-------------------------------------------------------------------------------

distrThesePair :: These (a, b) c -> (These a c, These b c)
distrThesePair (This (a, b))    = (This a, This b)
distrThesePair (That c)         = (That c, That c)
distrThesePair (These (a, b) c) = (These a c, These b c)

undistrThesePair :: (These a c, These b c) -> These (a, b) c
undistrThesePair (This a,    This b)    = This (a, b)
undistrThesePair (That c,    That _)    = That c
undistrThesePair (These a c, These b _) = These (a, b) c
undistrThesePair (This _,    That c)    = That c
undistrThesePair (This a,    These b c) = These (a, b) c
undistrThesePair (That c,    This _)    = That c
undistrThesePair (That c,    These _ _) = That c
undistrThesePair (These a c, This b)    = These (a, b) c
undistrThesePair (These _ c, That _)    = That c


distrPairThese :: (These a b, c) -> These (a, c) (b, c)
distrPairThese (This a,    c) = This (a, c)
distrPairThese (That b,    c) = That (b, c)
distrPairThese (These a b, c) = These (a, c) (b, c)

undistrPairThese :: These (a, c) (b, c) -> (These a b, c)
undistrPairThese (This (a, c))         = (This a, c)
undistrPairThese (That (b, c))         = (That b, c)
undistrPairThese (These (a, c) (b, _)) = (These a b, c)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------



instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
    This  a   <> This  b   = This  (a <> b)
    This  a   <> That    y = These  a             y
    This  a   <> These b y = These (a <> b)       y
    That    x <> This  b   = These       b   x
    That    x <> That    y = That           (x <> y)
    That    x <> These b y = These       b  (x <> y)
    These a x <> This  b   = These (a <> b)  x
    These a x <> That    y = These  a       (x <> y)
    These a x <> These b y = These (a <> b) (x <> y)

instance Functor (These a) where
    fmap _ (This x) = This x
    fmap f (That y) = That (f y)
    fmap f (These x y) = These x (f y)

instance Foldable (These a) where
    foldr _ z (This _) = z
    foldr f z (That x) = f x z
    foldr f z (These _ x) = f x z

instance Traversable (These a) where
    traverse _ (This a) = pure $ This a
    traverse f (That x) = That <$> f x
    traverse f (These a x) = These a <$> f x
    sequenceA (This a) = pure $ This a
    sequenceA (That x) = That <$> x
    sequenceA (These a x) = These a <$> x

instance Bifunctor These where
    bimap f _ (This  a  ) = This (f a)
    bimap _ g (That    x) = That (g x)
    bimap f g (These a x) = These (f a) (g x)

instance Bifoldable These where
    bifold = these id id mappend
    bifoldr f g z = these (`f` z) (`g` z) (\x y -> x `f` (y `g` z))
    bifoldl f g z = these (z `f`) (z `g`) (\x y -> (z `f` x) `g` y)

instance Bitraversable These where
    bitraverse f _ (This x) = This <$> f x
    bitraverse _ g (That x) = That <$> g x
    bitraverse f g (These x y) = These <$> f x <*> g y

instance (Semigroup a) => Applicative (These a) where
    pure = That
    This  a   <*> _         = This a
    That    _ <*> This  b   = This b
    That    f <*> That    x = That (f x)
    That    f <*> These b x = These b (f x)
    These a _ <*> This  b   = This (a <> b)
    These a f <*> That    x = These a (f x)
    These a f <*> These b x = These (a <> b) (f x)


instance (Semigroup a) => Monad (These a) where
    return = pure
    This  a   >>= _ = This a
    That    x >>= k = k x
    These a x >>= k = case k x of
                          This  b   -> This  (a <> b)
                          That    y -> These a y
                          These b y -> These (a <> b) y
instance (Hashable a, Hashable b) => Hashable (These a b)

-------------------------------------------------------------------------------
-- assoc
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_assoc
-- | @since 0.8
instance Swap These where
    swap (This a)    = That a
    swap (That b)    = This b
    swap (These a b) = These b a

-- | @since 0.8
instance Assoc These where
    assoc (This (This a))       = This a
    assoc (This (That b))       = That (This b)
    assoc (That c)              = That (That c)
    assoc (These (That b) c)    = That (These b c)
    assoc (This (These a b))    = These a (This b)
    assoc (These (This a) c)    = These a (That c)
    assoc (These (These a b) c) = These a (These b c)

    unassoc (This a)              = This (This a)
    unassoc (That (This b))       = This (That b)
    unassoc (That (That c))       = That c
    unassoc (That (These b c))    = These (That b) c
    unassoc (These a (This b))    = This (These a b)
    unassoc (These a (That c))    = These (This a) c
    unassoc (These a (These b c)) = These (These a b) c
#endif

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

-- | @since 0.7.1
instance (NFData a, NFData b) => NFData (These a b) where
    rnf (This a)    = rnf a
    rnf (That b)    = rnf b
    rnf (These a b) = rnf a `seq` rnf b

-------------------------------------------------------------------------------
-- binary
-------------------------------------------------------------------------------

-- | @since 0.7.1
instance (Binary a, Binary b) => Binary (These a b) where
    put (This a)    = put (0 :: Int) >> put a
    put (That b)    = put (1 :: Int) >> put b
    put (These a b) = put (2 :: Int) >> put a >> put b

    get = do
        i <- get
        case (i :: Int) of
            0 -> This <$> get
            1 -> That <$> get
            2 -> These <$> get <*> get
            _ -> fail "Invalid These index"
