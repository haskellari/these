-----------------------------------------------------------------------------
-- | Module     :  Data.These
--
-- The 'These' type and associated operations. Now enhanced with "Control.Lens" magic!
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
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
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq              (NFData (..))
import Control.Lens                 (Swapped (..), iso)
import Data.Aeson                   (FromJSON (..), ToJSON (..), (.=))
import Data.Bifoldable              (Bifoldable (..))
import Data.Bifunctor               (Bifunctor (..))
import Data.Bifunctor.Assoc         (Assoc (..))
import Data.Bifunctor.Swap          (Swap (..))
import Data.Binary                  (Binary (..))
import Data.Bitraversable           (Bitraversable (..))
import Data.Data                    (Data, Typeable)
import Data.Functor.Bind            (Apply (..), Bind (..))
import Data.Hashable                (Hashable (..))
import Data.Semigroup               (Semigroup (..))
import Data.Semigroup.Bifoldable    (Bifoldable1 (..))
import Data.Semigroup.Bitraversable (Bitraversable1 (..))
import GHC.Generics                 (Generic)
import Test.QuickCheck
       (Arbitrary (..), Arbitrary1 (..), Arbitrary2 (..), CoArbitrary (..),
       arbitrary1, oneof, shrink1)
import Test.QuickCheck.Function     (Function (..), functionMap)

import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Encoding as Aeson (pair)
import qualified Data.HashMap.Strict as HM

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
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

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

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

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

instance Bifoldable1 These where
    bifold1 = these id id (<>)

instance Bitraversable These where
    bitraverse f _ (This x) = This <$> f x
    bitraverse _ g (That x) = That <$> g x
    bitraverse f g (These x y) = These <$> f x <*> g y

instance Bitraversable1 These where
    bitraverse1 f _ (This x) = This <$> f x
    bitraverse1 _ g (That x) = That <$> g x
    bitraverse1 f g (These x y) = These <$> f x <.> g y

-- | @since 0.7.6
instance Swapped These where
    swapped = iso swap swap

instance (Semigroup a) => Apply (These a) where
    This  a   <.> _         = This a
    That    _ <.> This  b   = This b
    That    f <.> That    x = That (f x)
    That    f <.> These b x = These b (f x)
    These a _ <.> This  b   = This (a <> b)
    These a f <.> That    x = These a (f x)
    These a f <.> These b x = These (a <> b) (f x)

instance (Semigroup a) => Applicative (These a) where
    pure = That
    (<*>) = (<.>)

instance (Semigroup a) => Bind (These a) where
    This  a   >>- _ = This a
    That    x >>- k = k x
    These a x >>- k = case k x of
                          This  b   -> This  (a <> b)
                          That    y -> These a y
                          These b y -> These (a <> b) y

instance (Semigroup a) => Monad (These a) where
    return = pure
    (>>=) = (>>-)

instance (Hashable a, Hashable b) => Hashable (These a b)

-- | @since 0.7.1
instance (NFData a, NFData b) => NFData (These a b) where
    rnf (This a)    = rnf a
    rnf (That b)    = rnf b
    rnf (These a b) = rnf a `seq` rnf b

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

-- | @since 0.7.1
instance (ToJSON a, ToJSON b) => ToJSON (These a b) where
    toJSON (This a)    = Aeson.object [ "This" .= a ]
    toJSON (That b)    = Aeson.object [ "That" .= b ]
    toJSON (These a b) = Aeson.object [ "This" .= a, "That" .= b ]

    toEncoding (This a)    = Aeson.pairs $ "This" .= a
    toEncoding (That b)    = Aeson.pairs $ "That" .= b
    toEncoding (These a b) = Aeson.pairs $ "This" .= a <> "That" .= b

-- | @since 0.7.1
instance (FromJSON a, FromJSON b) => FromJSON (These a b) where
    parseJSON = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> parseJSON b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> parseJSON b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> parseJSON b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 0.7.2
instance Aeson.ToJSON2 These where
    liftToJSON2  toa _ _tob _ (This a)    = Aeson.object [ "This" .= toa a ]
    liftToJSON2 _toa _  tob _ (That b)    = Aeson.object [ "That" .= tob b ]
    liftToJSON2  toa _  tob _ (These a b) = Aeson.object [ "This" .= toa a, "That" .= tob b ]

    liftToEncoding2  toa _ _tob _ (This a)    = Aeson.pairs $ Aeson.pair "This" (toa a)
    liftToEncoding2 _toa _  tob _ (That b)    = Aeson.pairs $ Aeson.pair "That" (tob b)
    liftToEncoding2  toa _  tob _ (These a b) = Aeson.pairs $ Aeson.pair "This" (toa a) <> Aeson.pair "That" (tob b)

-- | @since 0.7.2
instance ToJSON a => Aeson.ToJSON1 (These a) where
    liftToJSON _tob _ (This a)    = Aeson.object [ "This" .= a ]
    liftToJSON  tob _ (That b)    = Aeson.object [ "That" .= tob b ]
    liftToJSON  tob _ (These a b) = Aeson.object [ "This" .= a, "That" .= tob b ]

    liftToEncoding _tob _ (This a)    = Aeson.pairs $ "This" .= a
    liftToEncoding  tob _ (That b)    = Aeson.pairs $ Aeson.pair "That" (tob b)
    liftToEncoding  tob _ (These a b) = Aeson.pairs $ "This" .= a <> Aeson.pair "That" (tob b)

-- | @since 0.7.2
instance Aeson.FromJSON2 These where
    liftParseJSON2 pa _ pb _ = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> pa a <*> pb b
        p [("That", b), ("This", a)] = These <$> pa a <*> pb b
        p [("This", a)] = This <$> pa a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 0.7.2
instance FromJSON a => Aeson.FromJSON1 (These a) where
    liftParseJSON pb _ = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> pb b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> pb b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 0.7.4
instance Arbitrary2 These where
    liftArbitrary2 arbA arbB = oneof
        [ This <$> arbA
        , That <$> arbB
        , These <$> arbA <*> arbB
        ]

    liftShrink2  shrA _shrB (This x) = This <$> shrA x
    liftShrink2 _shrA  shrB (That y) = That <$> shrB y
    liftShrink2  shrA  shrB (These x y) =
        [This x, That y] ++ [These x' y' | (x', y') <- liftShrink2 shrA shrB (x, y)]

-- | @since 0.7.4
instance (Arbitrary a) => Arbitrary1 (These a) where
    liftArbitrary = liftArbitrary2 arbitrary
    liftShrink = liftShrink2 shrink

-- | @since 0.7.1
instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
    arbitrary = arbitrary1
    shrink = shrink1

-- | @since 0.7.1
instance (Function a, Function b) => Function (These a b) where
  function = functionMap g f
    where
      g (This a)    = Left a
      g (That b)    = Right (Left b)
      g (These a b) = Right (Right (a, b))

      f (Left a)               = This a
      f (Right (Left b))       = That b
      f (Right (Right (a, b))) = These a b

-- | @since 0.7.1
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b)
