{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Tests.AlignWrong where

import Prelude ()
import Prelude.Compat hiding (zip, zipWith)

import Data.Map        (Map)
import Test.QuickCheck (Arbitrary (..))

import qualified Data.Map as Map

import Data.Semialign
import Data.These

---------------------------------------------------------------------------
-- WrongMap doesn't satisfy Align laws
-------------------------------------------------------------------------------

newtype WrongMap k v = WM (Map k v) deriving (Eq, Ord, Show, Functor, Foldable)

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (WrongMap k v) where
    arbitrary = WM <$> arbitrary
    shrink (WM m) = WM <$> shrink m

instance Ord k => Align (WrongMap k) where
    nil = WM Map.empty

instance Ord k => Semialign (WrongMap k) where
    align (WM x) (WM y)
       | Map.null y = WM $ This <$> x
       | Map.null x = WM $ That <$> y
       | otherwise  = WM $ Map.intersectionWith These x y

    zip (WM x) (WM y) = WM (Map.intersectionWith (,) x y)

-------------------------------------------------------------------------------
-- WeirdMap
-------------------------------------------------------------------------------

-- | Sequence-like __invalid__ 'Align' instance for Map.
--
-- Satisfies first five laws;
-- Doesn't satisfy /assoc/ or /toList/ laws.
--
newtype WeirdMap k v = WeirdMap (Map k v)
  deriving (Eq, Ord, Show, Functor, Foldable)

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (WeirdMap k v) where
    arbitrary = WeirdMap <$> arbitrary
    shrink (WeirdMap m) = WeirdMap <$> shrink m

instance Ord k => Align (WeirdMap k) where
    nil = WeirdMap Map.empty

instance Ord k => Semialign (WeirdMap k) where
    alignWith f (WeirdMap x) (WeirdMap y) = WeirdMap $ Map.fromList $
        alignWith g (Map.toList x) (Map.toList y)
      where
        g (This (k, a))         = (k, f (This a))
        g (That (k, a))         = (k, f (That a))
        g (These (k, a) (_, b)) = (k, f (These a b))

    zipWith f (WeirdMap x) (WeirdMap y) = WeirdMap $ Map.fromList $
        zipWith (\(k, a) (_, b) -> (k, f a b)) (Map.toList x) (Map.toList y)

-------------------------------------------------------------------------------
-- Const is invalid Align with Monoid, we need Idempotent monoid!
-------------------------------------------------------------------------------

{-
instance Monoid a => Align (Const a) where
    nil = Const mempty

instance Monoid a => Semialign (Const a) where
    align (Const a) (Const b) = Const (mappend a b)
-}

-------------------------------------------------------------------------------
-- R does satisfy Align laws, though is weird
-- https://github.com/isomorphism/these/issues/96
-------------------------------------------------------------------------------

newtype R a = Nest [[a]]
  deriving (Show, Eq, Ord, Functor, Foldable)

instance Align R where
    nil = Nest []

instance Semialign R where
    align (Nest ass) (Nest bss)
        | null ass                = That <$> Nest bss
        | null bss                = This <$> Nest ass
        | shape ass == shape bss  = Nest $ zipWith (zipWith These) ass bss
        | otherwise               = Nest [align (concat ass) (concat bss)]
      where
        shape = fmap (() <$)

    -- doesn't work with align above
    zip (Nest ass) (Nest bss) = Nest $ zipWith (zipWith (,)) ass bss

instance Arbitrary a => Arbitrary (R a) where
    arbitrary = Nest <$> arbitrary
    shrink (Nest xss) = Nest <$> shrink xss
