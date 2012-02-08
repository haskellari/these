-----------------------------------------------------------------------------
-- | Module     :  Data.These
--
--   The 'These' type, with associated operations.
module Data.These where

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable
import Data.Traversable
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..), Monoid(..))
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

-- | Unzips a list of 'These' values into parallel lists, inserting 'Nothing' 
--   in place of missing values. The resulting lists thus contain the same 
--   information as the original.
unzipThese :: [These a b] -> ([Maybe a], [Maybe b])
unzipThese = foldr (these a b ab) ([],[]) 
  where a  l   ~(ls,rs) = (Just l :ls, Nothing:rs)
        b    r ~(ls,rs) = (Nothing:ls, Just r :rs)
        ab l r ~(ls,rs) = (Just l :ls, Just r :rs)

-- | Zips two lists, padding to the length of the longer list. 
zipThese :: [a] -> [b] -> [These a b]
zipThese xs [] = This <$> xs
zipThese [] ys = That <$> ys
zipThese (x:xs) (y:ys) = These x y : zipThese xs ys

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

instance (Monoid a) => Applicative (These a) where
    pure = That
    This  a   <*> _         = This a
    That    f <*> This  b   = This b
    That    f <*> That    x = That (f x)
    That    f <*> These b x = These b (f x)
    These a f <*> This  b   = This (mappend a b)
    These a f <*> That    x = These a (f x)
    These a f <*> These b x = These (mappend a b) (f x)

instance (Monoid a) => Monad (These a) where
    return = pure
    This  a   >>= _ = This a
    That    x >>= k = k x
    These a x >>= k = case k x of 
                          This  b   -> This  (mappend a b)
                          That    y -> These a y
                          These b y -> These (mappend a b) y

