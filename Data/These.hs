-----------------------------------------------------------------------------
-- | Module     :  Data.These
--
-- The 'These' type and associated operations. Now enhanced with @Control.Lens@ magic!
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.These (
                    These(..)
                    
                  -- * Functions to get rid of 'These'
                  , these
                  , fromThese
                  , mergeThese
                  , mergeTheseWith
                  
                  -- * Traversals
                  , here, there
                  
                  -- * Prisms
                  , _This, _That, _These
                  
                  -- * Case selections
                  , justThis
                  , justThat
                  , justThese
                  
                  , catThis
                  , catThat
                  , catThese
                  
                  , partitionThese
                                    
                  -- * Case predicates
                  , isThis
                  , isThat
                  , isThese
                  
                  -- * Map operations
                  , mapThese
                  , mapThis
                  , mapThat
                  
                    -- $align
                  ) where

import Control.Applicative
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Bind
import Data.Maybe (isJust, mapMaybe)
import Data.Profunctor
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Traversable
import Data.Data
import GHC.Generics
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
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

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

-- | Coalesce with the provided operation.
mergeThese :: (a -> a -> a) -> These a a -> a
mergeThese = these id id

-- | BiMap and coalesce results with the provided operation.
mergeTheseWith :: (a -> c) -> (b -> c) -> (c -> c -> c) -> These a b -> c
mergeTheseWith f g op t = mergeThese op $ mapThese f g t


-- | A @Traversal@ of the first half of a 'These', suitable for use with @Control.Lens@.
here :: (Applicative f) => (a -> f b) -> These a t -> f (These b t)
here f (This x) = This <$> f x
here f (These x y) = flip These y <$> f x
here _ (That x) = pure (That x)

-- | A @Traversal@ of the second half of a 'These', suitable for use with @Control.Lens@.
there :: (Applicative f) => (a -> f b) -> These t a -> f (These t b)
there _ (This x) = pure (This x)
there f (These x y) = These x <$> f y
there f (That x) = That <$> f x

-- <cmccann> is there a recipe for creating suitable definitions anywhere?
-- <edwardk> not yet
-- <edwardk> prism bt seta = dimap seta (either pure (fmap bt)) . right'
-- (let's all pretend I know how this works ok)
prism :: (Choice p, Applicative f) => (b -> t) -> (s -> Either t a) -> p a (f b) -> p s (f t)
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- | A 'Prism' selecting the 'This' constructor.
_This :: (Choice p, Applicative f) => p a (f a) -> p (These a b) (f (These a b))
_This = prism This (these Right (Left . That) (\x y -> Left $ These x y))

-- | A 'Prism' selecting the 'That' constructor.
_That :: (Choice p, Applicative f) => p b (f b) -> p (These a b) (f (These a b))
_That = prism That (these (Left . This) Right (\x y -> Left $ These x y))

-- | A 'Prism' selecting the 'These' constructor. 'These' names are ridiculous!
_These :: (Choice p, Applicative f) => p (a, b) (f (a, b)) -> p (These a b) (f (These a b))
_These = prism (uncurry These) (these (Left . This) (Left . That) (\x y -> Right (x, y)))


-- | @'justThis' = preview '_This'@
justThis :: These a b -> Maybe a
justThis (This a) = Just a
justThis _        = Nothing

-- | @'justThat' = preview '_That'@
justThat :: These a b -> Maybe b
justThat (That x) = Just x
justThat _        = Nothing

-- | @'justThese' = preview '_These'@
justThese :: These a b -> Maybe (a, b)
justThese (These a x) = Just (a, x)
justThese _           = Nothing


isThis, isThat, isThese :: These a b -> Bool
-- | @'isThis' = 'isJust' . 'justThis'@
isThis  = isJust . justThis

-- | @'isThat' = 'isJust' . 'justThat'@
isThat  = isJust . justThat

-- | @'isThese' = 'isJust' . 'justThese'@
isThese = isJust . justThese

-- | 'Bifunctor' map.
mapThese :: (a -> c) -> (b -> d) -> These a b -> These c d
mapThese f _ (This  a  ) = This (f a)
mapThese _ g (That    x) = That (g x)
mapThese f g (These a x) = These (f a) (g x)

-- | @'mapThis' = over 'here'@
mapThis :: (a -> c) -> These a b -> These c b
mapThis f = mapThese f id

-- | @'mapThat' = over 'there'@
mapThat :: (b -> d) -> These a b -> These a d
mapThat f = mapThese id f

-- | Select all 'This' constructors from a list.
catThis :: [These a b] -> [a]
catThis = mapMaybe justThis

-- | Select all 'That' constructors from a list.
catThat :: [These a b] -> [b]
catThat = mapMaybe justThat

-- | Select all 'These' constructors from a list.
catThese :: [These a b] -> [(a, b)]
catThese = mapMaybe justThese

-- | Select each constructor and partition them into separate lists.
partitionThese :: [These a b] -> ( [(a, b)], ([a], [b]) )
partitionThese []             = ([], ([], []))
partitionThese (These x y:xs) = first ((x, y):)      $ partitionThese xs
partitionThese (This  x  :xs) = second (first  (x:)) $ partitionThese xs
partitionThese (That    y:xs) = second (second (y:)) $ partitionThese xs


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
