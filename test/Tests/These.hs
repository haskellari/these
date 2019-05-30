{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Tests.These where

import Prelude ()
import Prelude.Compat

import Control.Applicative       (liftA)
import Control.Monad             (ap, liftM)
import Data.Functor.Compose      (Compose (..))
import Data.Functor.Identity     (Identity (..))
import Data.List                 (nub)
import Data.List.NonEmpty        (NonEmpty)
import Data.Semigroup            (Semigroup (..))
import Data.Traversable          (fmapDefault, foldMapDefault)
import Data.Typeable             (Typeable, typeOf, typeOf1)
import Test.QuickCheck           (Arbitrary (..), Property, (===))
import Test.QuickCheck.Function  (Fun (..), applyFun)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Poly      (A, B, C)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import qualified Data.Aeson  as Aeson
import qualified Data.Binary as Binary
import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map

import Data.Align
import Data.These

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#else
import Data.Typeable (Typeable1)
#endif

theseProps :: TestTree
theseProps = testGroup "These"
    [ functorLaws (CTraversable :: CFunctor (These Int))
    , functorLaws (CMonad       :: CFunctor (These (NonEmpty Int)))
    , testProperty "Map value laziness property" mapStrictnessProp
    , testProperty "IntMap value laziness property" intmapStrictnessProp
    , aesonProps
    , binaryProps
    , semigroupLaws  (CSemigroup :: CSemigroup (These String String))
    , testGroup "Extras"
        [ semigroupLaws (CSemigroup :: CSemigroup (SearchResult String String))
        , semigroupLaws (CMonoid    :: CSemigroup String)
        ]
    ]

-------------------------------------------------------------------------------
-- SearchResult
-------------------------------------------------------------------------------

-- | Either a, or b, or both a and b
--
-- See https://github.com/isomorphism/these/issues/80
data SearchResult a b = Scanned a | Found b | ScannedAndFound a b
  deriving (Eq, Ord, Show, Typeable)

instance (Arbitrary a, Arbitrary b) => Arbitrary (SearchResult a b) where
    arbitrary = srFromThese <$> arbitrary

srFromThese :: These a b -> SearchResult a b
srFromThese (This a)    = Scanned a
srFromThese (That b)    = Found b
srFromThese (These a b) = ScannedAndFound a b

-- | Accumulate 'a's from left to right, until one 'b' is found
instance Semigroup a => Semigroup (SearchResult a b) where
    ScannedAndFound a b <> _ = ScannedAndFound a b
    Found b <> _ = Found b
    Scanned a <> Scanned a' = Scanned (a <> a')
    Scanned a <> Found b = ScannedAndFound a b
    Scanned a <> ScannedAndFound a' b = ScannedAndFound (a <> a') b

-------------------------------------------------------------------------------
-- Semigroup & Monoid Laws
-------------------------------------------------------------------------------

data CSemigroup a where
    CSemigroup :: Semigroup a             => CSemigroup a
    CMonoid    :: (Semigroup a, Monoid a) => CSemigroup a

semigroupLaws
    :: forall a. (Typeable a, Show a, Eq a, Arbitrary a)
    => CSemigroup a
    -> TestTree
semigroupLaws c = testGroup name $ case c of
    CSemigroup -> [semigroupLaws' c]
    CMonoid    -> [semigroupLaws' c, monoidLaws' c]
  where
    name = show (typeOf (undefined :: a))

semigroupLaws'
    :: forall a proxy. (Semigroup a, Show a, Eq a, Arbitrary a)
    => proxy a -> TestTree
semigroupLaws' _ = testGroup "Semigroup"
    [ testProperty "associativity" assocProp
    ]
  where
    assocProp :: a -> a -> a -> Property
    assocProp x y z = (x <> y) <> z === x <> (y <> z)

monoidLaws'
    :: forall a proxy. (Semigroup a, Monoid a, Show a, Eq a, Arbitrary a)
    => proxy a -> TestTree
monoidLaws' _ = testGroup "Monoid"
    [ testProperty "associativity" assocProp
    , testProperty "left-identity" idLeftProp
    , testProperty "right-identity" idRightProp
    , testProperty "mappend = (<>)" mappendDefProp
    ]
  where
    assocProp :: a -> a -> a -> Property
    assocProp x y z = (x `mappend` y) `mappend` z === x `mappend` (y `mappend` z)

    idLeftProp :: a -> Property
    idLeftProp x = mappend mempty x === x

    idRightProp :: a -> Property
    idRightProp x = mappend x mempty === x

    mappendDefProp :: a -> a -> Property
    mappendDefProp x y = mappend x y === (x <> y)

-------------------------------------------------------------------------------
-- Functor .. Traversable
-------------------------------------------------------------------------------

data CFunctor f where
    CFunctor     :: Functor f                => CFunctor f
    CTraversable :: Traversable f            => CFunctor f
    CApplicative :: Applicative f            => CFunctor f
    CMonad       :: (Applicative f, Monad f) => CFunctor f

functorLaws
    :: forall f. (Typeable1 f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Show (f (Fun A B)), Arbitrary (f (Fun A B))
       , Show (f (Fun B C)), Arbitrary (f (Fun B C))
       )
    => CFunctor f
    -> TestTree
functorLaws c = testGroup name $ case c of
    CFunctor     -> [functorLaws' c]
    CTraversable -> [functorLaws' c, traversableLaws' c]
    CApplicative -> [functorLaws' c, applicativeLaws' c]
    CMonad       -> [functorLaws' c, applicativeLaws' c, monadLaws' c]
  where
    name = show (typeOf1 (undefined :: f ()))

functorLaws'
    :: forall f proxy. ( Functor f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f C), Show (f C), Arbitrary (f C)
       )
    => proxy f -> TestTree
functorLaws' _ = testGroup "Functor"
    [ testProperty "identity" functorIdentityProp
    , testProperty "composition" functorCompositionProp
    ]
  where
    functorIdentityProp :: f A -> Property
    functorIdentityProp x = fmap id x === x

    functorCompositionProp :: f A -> Fun A B -> Fun B C -> Property
    functorCompositionProp x (Fun _ f) (Fun _ g) = fmap g (fmap f x) === fmap (g . f) x

traversableLaws'
    :: forall f proxy. ( Traversable f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       )
    => proxy f -> TestTree
traversableLaws' _ = testGroup "Traversable"
    [ testProperty "identity"    traversableIdentityProp
    , testProperty "identity'"   traversableIdentityProp'
    , testProperty "composition" traversableCompositionProp
    , testProperty "functor"     traversableFunctorProp
    , testProperty "foldable"    traversableFoldableProp
    ]
  where
    traversableIdentityProp :: f A -> Property
    traversableIdentityProp x = traverse Identity x === Identity x

    traversableIdentityProp' :: f A -> Property
    traversableIdentityProp' x = traverse pure x === (pure x :: Maybe (f A))

    traversableCompositionProp :: f A -> Fun A (Maybe B) -> Fun B (Either Bool C) -> Property
    traversableCompositionProp x (Fun _ f) (Fun _ g) = traverse (Compose . fmap g . f) x === (Compose . fmap (traverse g) . traverse f $ x)

    traversableFunctorProp :: f A -> Fun A B -> Property
    traversableFunctorProp x (Fun _ f) = fmap f x === fmapDefault f x

    traversableFoldableProp :: f A -> Fun A [B] -> Property
    traversableFoldableProp x (Fun _ f) = foldMap f x === foldMapDefault f x

applicativeLaws'
    :: forall f proxy. ( Applicative f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Show (f (Fun A B)), Arbitrary (f (Fun A B))
       , Show (f (Fun B C)), Arbitrary (f (Fun B C))
       )
    => proxy f -> TestTree
applicativeLaws' _ = testGroup "Applicative"
    [ testProperty "identity" identity
    , testProperty "composition" composition
    , testProperty "homomorphism" homomorphism
    , testProperty "interchange" interchange
    , testProperty "fmap = liftA"  fmapLiftA
    ]
  where
    identity :: f A -> Property
    identity v = (pure id <*> v) === v

    composition :: f (Fun B C) -> f (Fun A B) -> f A -> Property
    composition u' v' w = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w)) where
        u = fmap applyFun u'
        v = fmap applyFun v'

    homomorphism :: Fun A B -> A -> Property
    homomorphism (Fun _ f) x = (pure f <*> pure x) === (pure (f x) :: f B)

    interchange :: f (Fun A B) -> A -> Property
    interchange u' y = (u <*> pure y) === (pure ($ y) <*> u)
      where
        u = fmap applyFun u'

    fmapLiftA :: f A -> Fun A B -> Property
    fmapLiftA x (Fun _ f) = fmap f x === liftA f x

monadLaws'
    :: forall f proxy. ( Monad f, Applicative f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Show (f (Fun A B)), Arbitrary (f (Fun A B))
       )
    => proxy f -> TestTree
monadLaws' _ = testGroup "Monad"
    [ testProperty "right identity" rightIdentity
    , testProperty "left identity"  leftIdentity
    , testProperty "composition"    composition
    , testProperty "pure = return"  pureReturn
    , testProperty "(<*>) = ap"     apAp
    , testProperty "fmap = liftM "  fmapLiftM
    ]
  where
    rightIdentity :: f A -> Property
    rightIdentity m = (m >>= return) === m

    leftIdentity :: A -> Fun A (f B) -> Property
    leftIdentity x (Fun _ k) = (return x >>= k) === k x

    composition :: f A -> Fun A (f B) -> Fun B (f C) -> Property
    composition m (Fun _ f) (Fun _ g) = lhs === rhs where
        lhs = (m >>= f) >>= g
        rhs = m >>= (\x -> f x >>= g)

    pureReturn :: A -> Property
    pureReturn x = pure x === (return x :: f A)

    apAp :: f (Fun A B) -> f A -> Property
    apAp f' x = (f <*> x) === ap f x where
        f = fmap applyFun f'

    fmapLiftM :: f A -> Fun A B -> Property
    fmapLiftM x (Fun _ f) = fmap f x === liftM f x

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

-------------------------------------------------------------------------------
-- Strictness woes
-------------------------------------------------------------------------------

-- Even the `align` is/was defined using strict combinators, this will still work:
mapStrictnessProp :: [Int] -> [Int] -> Bool
mapStrictnessProp lkeys rkeys = length (nub lkeys) <= Map.size (lhs `align` rhs)
  where lhs  = Map.fromList $ fmap (,loop) lkeys
        rhs  = Map.fromList $ fmap (,loop) rkeys

        loop :: Int
        loop = error "break"

intmapStrictnessProp :: [Int] -> [Int] -> Bool
intmapStrictnessProp lkeys rkeys = length (nub lkeys) <= IntMap.size (lhs `align` rhs)
  where lhs  = IntMap.fromList $ fmap (,loop) lkeys
        rhs  = IntMap.fromList $ fmap (,loop) rkeys

        loop :: Int
        loop = error "break"
