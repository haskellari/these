-----------------------------------------------------------------------------
-- | Module     :  Data.These
--
-- The 'These' type, with associated operations.
module Data.These (
                    These(..)
                  , these
                  , fromThese
                  , justThis
                  , justThat
                  , justThese
                  , isThis
                  , isThat
                  , isThese
                  , mapThese
                  , mapThis
                  , mapThat
                  , catThese
                  , catThis
                  , catThat
                    -- $align
                  ) where

import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Bind
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..), Monoid(..))
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Traversable
import Prelude hiding (foldr)

-- --------------------------------------------------------------------------
-- | The 'These' type represents values with two non-exclusive possibilities.
--
--   This can be useful to represent combinations of two values, where the 
--   combination is defined if either input is. Algebraically, the type 
--   @These A B@ represents @(A + B + AB)@, which doesn't factor easily into
--   sums and products--a type like @Either A (B, Maybe A)@ is unclear and
--   awkward to use.
--
--   'These' has straightforward instances of 'Functor', 'Monad', &c., and 
--   behaves like a hybrid error/writer monad, as would be expected.
data These a b = This a | That b | These a b
    deriving (Eq, Ord, Read, Show)

-- | Case analysis for the 'These' type.
these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
these l _ _ (This a) = l a
these _ r _ (That x) = r x
these _ _ lr (These a x) = lr a x

-- | Takes two default values and produces a tuple.
fromThese :: a -> b -> These a b -> (a, b)
fromThese _ x (This a   ) = (a, x)
fromThese a _ (That x   ) = (a, x)
fromThese _ _ (These a x) = (a, x)

justThis :: These a b -> Maybe a
justThis (This a) = Just a
justThis _        = Nothing

justThat :: These a b -> Maybe b
justThat (That x) = Just x
justThat _        = Nothing

justThese :: These a b -> Maybe (a, b)
justThese (These a x) = Just (a, x)
justThese _           = Nothing

isThis, isThat, isThese :: These a b -> Bool
isThis  = isJust . justThis
isThat  = isJust . justThat
isThese = isJust . justThese

mapThese :: (a -> c) -> (b -> d) -> These a b -> These c d
mapThese f _ (This  a  ) = This (f a)
mapThese _ g (That    x) = That (g x)
mapThese f g (These a x) = These (f a) (g x)

mapThis :: (a -> c) -> These a b -> These c b
mapThis f = mapThese f id

mapThat :: (b -> d) -> These a b -> These a d
mapThat f = mapThese id f

catThese :: [These a a] -> [a]
catThese (This  x  :xs) = x     : catThese xs
catThese (That    y:xs) =     y : catThese xs
catThese (These x y:xs) = x : y : catThese xs

catThis :: [These a b] -> [a]
catThis (This x:xs) = x : catThis xs
catThis (_     :xs) =     catThis xs

catThat :: [These a b] -> [b]
catThat (That x:xs) = x : catThat xs
catThat (_     :xs) =     catThat xs

-- $align
--
-- For zipping and unzipping of structures with 'These' values, see
-- "Data.Align".

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
    foldr f z = foldr f z . justThat

instance Traversable (These a) where
    traverse _ (This a) = pure $ This a
    traverse f (That x) = That <$> f x
    traverse f (These a x) = These a <$> f x
    sequenceA (This a) = pure $ This a
    sequenceA (That x) = That <$> x
    sequenceA (These a x) = These a <$> x

instance Bifunctor These where
    bimap = mapThese
    first = mapThis
    second = mapThat

instance Bifoldable These where
    bifold = these id id mappend
    bifoldr f g z = these (`f` z) (`g` z) (\x y -> x `f` (y `g` z))
    bifoldl f g z = these (z `f`) (z `g`) (\x y -> (z `f` x) `g` y)

instance Bifoldable1 These where
    bifold1 = these id id (<>)

instance Bitraversable These where
    bitraverse f _ (This x) = This <$> f x
    bitraverse _ g (That x) = That <$> g x
    bitraverse f g (These x y) = These <$> f x <*> g y
    bimapM f _ (This x) = liftM This (f x)
    bimapM _ g (That x) = liftM That (g x)
    bimapM f g (These x y) = liftM2 These (f x) (g y)

instance Bitraversable1 These where
    bitraverse1 f _ (This x) = This <$> f x
    bitraverse1 _ g (That x) = That <$> g x
    bitraverse1 f g (These x y) = These <$> f x <.> g y

instance (Monoid a) => Apply (These a) where
    This  a   <.> _         = This a
    That    _ <.> This  b   = This b
    That    f <.> That    x = That (f x)
    That    f <.> These b x = These b (f x)
    These a _ <.> This  b   = This (mappend a b)
    These a f <.> That    x = These a (f x)
    These a f <.> These b x = These (mappend a b) (f x)

instance (Monoid a) => Applicative (These a) where
    pure = That
    (<*>) = (<.>)

instance (Monoid a) => Bind (These a) where
    This  a   >>- _ = This a
    That    x >>- k = k x
    These a x >>- k = case k x of
                          This  b   -> This  (mappend a b)
                          That    y -> These a y
                          These b y -> These (mappend a b) y

instance (Monoid a) => Monad (These a) where
    return = pure
    (>>=) = (>>-)
