{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Main (main) where

import Control.Applicative
import Control.Monad (join)
import Data.Align
import Data.Foldable
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Functor.Product as P
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Monoid
import Data.These
import Data.Traversable
import qualified Data.Vector as V
import Test.QuickCheck.Function
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- For old GHC to work
data Proxy (a :: * -> *) = Proxy

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [theseProps]

theseProps :: TestTree
theseProps = testGroup "These"
  [ functorProps
  , traversableProps
  , dataAlignLaws "[]" (Proxy :: Proxy [])
  , dataAlignLaws "IntMap" (Proxy :: Proxy IntMap)
  , dataAlignLaws "Map Char" (Proxy :: Proxy (Map Char))
  , dataAlignLaws "Maybe" (Proxy :: Proxy Maybe)
  , dataAlignLaws "Product [] Maybe" (Proxy :: Proxy (P.Product [] Maybe))
  , dataAlignLaws "Seq" (Proxy :: Proxy Seq)
  , dataAlignLaws "Vector" (Proxy :: Proxy V.Vector)
  , dataAlignLaws "ZipList" (Proxy :: Proxy ZipList)
  ]

functorIdentityProp :: (Functor f, Eq (f a), Show (f a)) => f a -> Property
functorIdentityProp x = fmap id x === x

functorCompositionProp :: (Functor f, Show (f c), Eq (f c)) => f a -> Fun a b -> Fun b c -> Property
functorCompositionProp x (Fun _ f) (Fun _ g) = fmap g (fmap f x) === fmap (g . f) x

functorProps :: TestTree
functorProps = testGroup "Functor"
  [ QC.testProperty "identity" (functorIdentityProp :: These Int Bool -> Property)
  , QC.testProperty "composition" (functorCompositionProp :: These Int Int -> Fun Int Int -> Fun Int Int -> Property)
  ]

traversableIdentityProp :: (Traversable t, Eq (t a), Show (t a)) => t a -> Property
traversableIdentityProp x = traverse Identity x === Identity x

traversableCompositionProp :: (Traversable t, Applicative g, Applicative f, Show (Compose f g (t b)), Eq (Compose f g (t b)))
                           => t a1 -> Fun a1 (f a) -> Fun a (g b) -> Property
traversableCompositionProp x (Fun _ f) (Fun _ g) = traverse (Compose . fmap g . f) x === (Compose . fmap (traverse g) . traverse f $ x)

traversableFunctorProp :: (Traversable f, Show (f b), Eq (f b)) => f a -> Fun a b -> Property
traversableFunctorProp x (Fun _ f) = fmap f x === fmapDefault f x

traversableFoldableProp :: (Monoid m, Traversable t, Show m, Eq m) => t a -> Fun a m -> Property
traversableFoldableProp x (Fun _ f) = foldMap f x === foldMapDefault f x

traversableProps :: TestTree
traversableProps = testGroup "Traversable"
  [ QC.testProperty "identity" (traversableIdentityProp :: These Int Bool -> Property)
  , QC.testProperty "composition" (traversableCompositionProp :: These Bool Int -> Fun Int (Maybe Int) -> Fun Int (Either Bool Int) -> Property)
  , QC.testProperty "functor" (traversableFunctorProp :: These Bool Int -> (Fun Int Int) -> Property)
  , QC.testProperty "foldable" (traversableFoldableProp :: These Bool Int -> (Fun Int [Bool]) -> Property)
  ]

-- Data.Align

-- (\`align` nil) = fmap This
-- (nil \`align`) = fmap That
-- join align = fmap (join These)
-- align (f \<$> x) (g \<$> y) = bimap f g \<$> align x y
-- alignWith f a b = f \<$> align a b

dataAlignLaws :: forall (f :: * -> *). ( Align f
                                       , Eq (f (These Int Int))
                                       , Show (f (These Int Int))
                                       , CoArbitrary (These Int Int)
                                       , Arbitrary (f Int)
                                       , Eq (f Int)
                                       , Show (f Int))
              => String
              -> Proxy f
              -> TestTree
dataAlignLaws name _ = testGroup ("Data.Align laws: " <> name)
  [ QC.testProperty "right identity" rightIdentityProp
  , QC.testProperty "left identity" leftIdentityProp
  , QC.testProperty "join" joinProp
  , QC.testProperty "bimap" bimapProp
  , QC.testProperty "alignWith" alignWithProp
  ]
  where rightIdentityProp :: f Int -> Property
        rightIdentityProp xs = (xs `align` (nil :: f Int)) === fmap This xs
        leftIdentityProp :: f Int -> Property
        leftIdentityProp xs = ((nil :: f Int) `align` xs) === fmap That xs
        joinProp :: f Int -> Property
        joinProp xs = join align xs === fmap (join These) xs
        bimapProp :: f Int -> f Int -> Fun Int Int -> Fun Int Int -> Property
        bimapProp xs ys (Fun _ f) (Fun _ g) =
          align (f <$> xs) (g <$> ys) === (bimap f g <$> align xs ys)
        alignWithProp :: f Int -> f Int -> Fun (These Int Int) Int -> Property
        alignWithProp xs ys (Fun _ f) =
          alignWith f xs ys === (f <$> align xs ys)

-- Orphan instances

instance (Arbitrary a, Arbitrary (f a), Arbitrary (g a))
    => Arbitrary (P.Product f g a) where
  arbitrary = P.Pair <$> arbitrary <*> arbitrary
  shrink (P.Pair x y) = [P.Pair x' y' | (x', y') <- shrink (x, y)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof [ This <$> arbitrary
                    , That <$> arbitrary
                    , These <$> arbitrary <*> arbitrary
                    ]
  shrink (This x)    = This <$> shrink x
  shrink (That y)    = That <$> shrink y
  shrink (These x y) = [This x, That y] ++
                       [These x' y' | (x', y') <- shrink (x, y)]

instance (Function a, Function b) => Function (These a b) where
  function = functionMap g f
    where
      g (This a)    = Left a
      g (That b)    = Right (Left b)
      g (These a b) = Right (Right (a, b))

      f (Left a)               = This a
      f (Right (Left b))       = That b
      f (Right (Right (a, b))) = These a b

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (These a b)

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = fmap V.fromList . shrink . V.toList

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary
  shrink = fmap ZipList . shrink . getZipList
