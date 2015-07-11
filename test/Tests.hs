{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Applicative
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
import Data.These
import Data.Traversable
import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [theseProps]

theseProps :: TestTree
theseProps = testGroup "These"
  [ functorProps
  , traversableProps
  ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof [ This <$> arbitrary
                    , That <$> arbitrary
                    , These <$> arbitrary <*> arbitrary
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
