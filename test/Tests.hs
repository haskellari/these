{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative
import Control.Monad (join)
import Data.Align
import Data.Align.Key
import Data.Foldable
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Functor.Product as P
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Monoid
import Data.These
import Data.Int (Int8)
import Data.Traversable
import qualified Data.Vector as V
import Prelude -- Fix redundant import warnings
import Test.QuickCheck.Function
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Set as Set

-- For old GHC to work
data Proxy (a :: * -> *) = Proxy

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [theseProps, alignWithKeyProps]

theseProps :: TestTree
theseProps = testGroup "These"
  [ functorProps
  , traversableProps
  , dataAlignLaws "[]" (Proxy :: Proxy [])
  , dataAlignLaws "HashMap String" (Proxy :: Proxy (HashMap String))
  , dataAlignLaws "IntMap" (Proxy :: Proxy IntMap)
  , dataAlignLaws "Map Char" (Proxy :: Proxy (Map Char))
  , dataAlignLaws "Maybe" (Proxy :: Proxy Maybe)
  , dataAlignLaws "Product [] Maybe" (Proxy :: Proxy (P.Product [] Maybe))
  , dataAlignLaws "Seq" (Proxy :: Proxy Seq)
  , dataAlignLaws "Vector" (Proxy :: Proxy V.Vector)
  , dataAlignLaws "ZipList" (Proxy :: Proxy ZipList)
  , crosswalkLaws "[]" (Proxy :: Proxy [])
  -- , crosswalkLaws "Identity" (Proxy :: Proxy Identity)
  , crosswalkLaws "Maybe" (Proxy :: Proxy Maybe)
  , crosswalkLaws "These" (Proxy :: Proxy (These Int))
  , crosswalkLaws "Seq" (Proxy :: Proxy Seq)
  , crosswalkLaws "Vector" (Proxy :: Proxy V.Vector)
  , testProperty "Map value laziness property" mapStrictnessProp
  , testProperty "IntMap value laziness property" intmapStrictnessProp
  , aesonProps
  , binaryProps
  ]

alignWithKeyProps :: TestTree
alignWithKeyProps = testGroup "AlignWithKey"
    [ testProperty "example" $ once $ example
    ]
  where
    example = alignWithKey (,) "foo" "quux" ===
        [ (0, These 'f' 'q')
        , (1, These 'o' 'u')
        , (2, These 'o' 'u')
        , (3, That 'x')
        ]

-- Even the `align` is defined using strict combinators, this will still work:
mapStrictnessProp :: [Int] -> [Int] -> Bool
mapStrictnessProp lkeys rkeys = Prelude.length (nub lkeys) <= Map.size (lhs `align` rhs)
  where lhs  = Map.fromList $ fmap (,loop) lkeys
        rhs  = Map.fromList $ fmap (,loop) rkeys
        loop :: Int
        loop = loop

intmapStrictnessProp :: [Int] -> [Int] -> Bool
intmapStrictnessProp lkeys rkeys = Prelude.length (nub lkeys) <= IntMap.size (lhs `align` rhs)
  where lhs  = IntMap.fromList $ fmap (,loop) lkeys
        rhs  = IntMap.fromList $ fmap (,loop) rkeys
        loop :: Int
        loop = loop

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

data Index = I1 | I2 | I3 | I4
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Index where
    arbitrary = elements [minBound .. maxBound]
    shrink I1 = []
    shrink I2 = [I1]
    shrink I3 = [I1, I2]
    shrink I4 = [I1, I2, I3]

crosswalkLaws
    :: forall (t :: * -> *).
       ( Crosswalk t
       , Arbitrary (t Int)
       , Eq (t Int), Show (t Int)
       )
    => String
    -> Proxy t
    -> TestTree
crosswalkLaws name _ = testGroup ("Data.CrossWalk laws: " <> name)
  [ QC.testProperty "crosswalk (const nil) = const nil" firstLaw
  , QC.testProperty "crosswalk f = sequenceL . fmap f" secondLaw
  ]
  where
    -- f = Map Index
    -- a, b = Int
    firstLaw :: t Int -> Property
    firstLaw x = lhs === rhs
      where
        lhs = crosswalk (const nil) x
        rhs = const nil x :: Map Index (t Int)

    secondLaw :: Fun Int (Map Index Int) -> t Int -> Property
    secondLaw (Fun _ f) x = lhs === rhs
      where
        lhs = crosswalk f x
        rhs = sequenceL . fmap f $ x

-------------------------------------------------------------------------------
-- Orphan instances
-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary (f a), Arbitrary (g a))
    => Arbitrary (P.Product f g a) where
  arbitrary = P.Pair <$> arbitrary <*> arbitrary
  shrink (P.Pair x y) = [P.Pair x' y' | (x', y') <- shrink (x, y)]


#if !MIN_VERSION_quickcheck_instances(0,3,12)
instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = fmap V.fromList . shrink . V.toList
#endif

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary
  shrink = fmap ZipList . shrink . getZipList
#endif

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

aesonProps :: TestTree
aesonProps = testGroup "aeson"
    [ testProperty "roundtrip / direct" prop1
    , testProperty "roundtrip / toJSON" prop2
    ]
  where
    prop1 :: These Int String -> Property
    prop1 x = Just x === Aeson.decode (Aeson.encode x)

    prop2 :: These Int String -> Property
    prop2 x = Just x === Aeson.decode (Aeson.encode $ Aeson.toJSON x)

-------------------------------------------------------------------------------
-- binary
-------------------------------------------------------------------------------

binaryProps :: TestTree
binaryProps = testProperty "binary / roundtrip" prop
  where
    prop :: These Int String -> Property
    prop x = x === Binary.decode (Binary.encode x)
