{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Applicative       (ZipList (..))
import Control.Lens              (folded, toListOf)
import Control.Monad             (join)
import Data.Bifunctor            (bimap)
import Data.Foldable             (toList)
import Data.Functor.Compose      (Compose (..))
import Data.Functor.Identity     (Identity (..))
import Data.HashMap.Strict       (HashMap)
import Data.IntMap               (IntMap)
import Data.List                 (nub)
import Data.List.NonEmpty        (NonEmpty)
import Data.Map                  (Map)
import Data.Maybe                (mapMaybe)
import Data.Semigroup            (Semigroup (..))
import Data.Sequence             (Seq)
import Data.Traversable          (fmapDefault, foldMapDefault)
import Test.QuickCheck
       (Arbitrary (..), CoArbitrary (..), Property, elements, once, (.&&.),
       (===))
import Test.QuickCheck.Function  (Fun (..))
import Test.QuickCheck.Instances ()
import Test.Tasty                (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import qualified Data.Aeson            as Aeson
import qualified Data.Binary           as Binary
import qualified Data.Functor.Product  as P
import qualified Data.IntMap           as IntMap
import qualified Data.Map              as Map
import qualified Data.Tree             as T
import qualified Data.Vector           as V
import qualified Test.Tasty.QuickCheck as QC

import Data.Align
import Data.Align.Indexed
import Data.Align.Key
import Data.These
import Data.These.Combinators
import Data.These.Lens

-- For old GHC to work
data Proxy (a :: * -> *) = Proxy

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ theseProps
    , alignProps
    , alignWithKeyProps
    , crosswalkProps
    , testGroup "Semigroup"
        [ semigroupLaws "These" (These "x" "y")
        , semigroupLaws "SearchResult" (ScannedAndFound "x" "y")
        , monoidLaws "List" "x" -- to disallow
        ]
    ]

theseProps :: TestTree
theseProps = testGroup "These"
    [ functorProps
    , traversableProps
    , testProperty "Map value laziness property" mapStrictnessProp
    , testProperty "IntMap value laziness property" intmapStrictnessProp
    , aesonProps
    , binaryProps
    ]

crosswalkProps :: TestTree
crosswalkProps = testGroup "Crosswalk"
    [ crosswalkLaws "[]" (Proxy :: Proxy [])
    -- , crosswalkLaws "Identity" (Proxy :: Proxy Identity)
    , crosswalkLaws "Maybe" (Proxy :: Proxy Maybe)
    , crosswalkLaws "These" (Proxy :: Proxy (These Int))
    , crosswalkLaws "Seq" (Proxy :: Proxy Seq)
    , crosswalkLaws "Vector" (Proxy :: Proxy V.Vector)
    , crosswalkLaws "(,) Int" (Proxy :: Proxy ((,) Int))
    , crosswalkLaws "Compose [] []" (Proxy :: Proxy (Compose [] []))
    ]

alignProps :: TestTree
alignProps = testGroup "Align"
    [ dataAlignLaws "[]"               (CAlign :: C [])
    , dataAlignLaws "HashMap String"   (CAlign :: C (HashMap String))
    , dataAlignLaws "IntMap"           (CAlign :: C IntMap)
    , dataAlignLaws "Map Char"         (CAlign :: C (Map Char))
    , dataAlignLaws "Maybe"            (CAlign :: C Maybe)
    , dataAlignLaws "Product [] Maybe" (CAlign :: C (P.Product [] Maybe))
    , dataAlignLaws "Compose [] Maybe" (CAlign :: C (Compose [] Maybe))
    , dataAlignLaws "Seq"              (CAlign :: C Seq)
    , dataAlignLaws "Vector"           (CAlign :: C V.Vector)
    , dataAlignLaws "ZipList"          (CAlign :: C ZipList)
    , dataAlignLaws "Tree"             (CSemialign :: C T.Tree)
    , dataAlignLaws "NonEmpty"         (CSemialign :: C NonEmpty)

    -- , dataAlignLaws "WrongMap" (CAlign :: C (WrongMap Char))
    -- weird objects:
    -- , dataAlignLaws "Const String" (CAlign :: C (Const String))
    , dataAlignLaws "R" (CAlign :: C R)
    -- , dataAlignLaws "Weirdmap" (CAlign :: C (WeirdMap Char))
    ]

alignWithKeyProps :: TestTree
alignWithKeyProps = testGroup "AlignWithKey / AlignWithIndex"
    [ testProperty "example" $ once $ exampleK
    , testProperty "example" $ once $ exampleI
    ]
  where
    exampleK = alignWithKey (,) "foo" "quux" === exampleV
    exampleI = ialign (,) "foo" "quux" === exampleV

    exampleV =
        [ (0, These 'f' 'q')
        , (1, These 'o' 'u')
        , (2, These 'o' 'u')
        , (3, That 'x')
        ]

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

-------------------------------------------------------------------------------
-- Align laws
-------------------------------------------------------------------------------

-- Data.Align

data C f where
    CSemialign :: Semialign f => C f
    CAlign     :: Align f     => C f

-- (\`align` nil) = fmap This
-- (nil \`align`) = fmap That
-- join align = fmap (join These)
-- align (f \<$> x) (g \<$> y) = bimap f g \<$> align x y
-- alignWith f a b = f \<$> align a b
--
-- We also require a sixth property, when f is Foldable.
dataAlignLaws
    :: forall (f :: * -> *).
       ( Foldable f
       , Eq (f (These Int Int))
       , Show (f (These Int Int))
       , Eq (f (These Int (These Int Int)))
       , Show (f (These Int (These Int Int)))
       , CoArbitrary (These Int Int)
       , Arbitrary (f Int)
       , Eq (f Int)
       , Show (f Int)
       )
    => String
    -> C f
    -> TestTree
dataAlignLaws name p =
    testGroup ("Data.Align laws: " <> name) props
  where
    props = case p of
        CSemialign -> semialignProps
        CAlign     -> semialignProps ++ alignLaws

    semialignProps :: Semialign f => [TestTree]
    semialignProps =
        [ QC.testProperty "join" joinProp
        , QC.testProperty "bimap" bimapProp
        , QC.testProperty "alignWith" alignWithProp
        , QC.testProperty "assoc" assocProp
        , QC.testProperty "alignToList" alignToListProp
        ]

    alignLaws :: Align f => [TestTree]
    alignLaws =
        [ QC.testProperty "right identity" rightIdentityProp
        , QC.testProperty "left identity" leftIdentityProp
        ]

    rightIdentityProp :: Align f => f Int -> Property
    rightIdentityProp xs = (xs `align` (nil :: f Int)) === fmap This xs

    leftIdentityProp :: Align f => f Int -> Property
    leftIdentityProp xs = ((nil :: f Int) `align` xs) === fmap That xs

    joinProp :: Semialign f => f Int -> Property
    joinProp xs = join align xs === fmap (join These) xs

    bimapProp :: Semialign f => f Int -> f Int -> Fun Int Int -> Fun Int Int -> Property
    bimapProp xs ys (Fun _ f) (Fun _ g) =
      align (f <$> xs) (g <$> ys) === (bimap f g <$> align xs ys)

    alignWithProp :: Semialign f => f Int -> f Int -> Fun (These Int Int) Int -> Property
    alignWithProp xs ys (Fun _ f) =
      alignWith f xs ys === (f <$> align xs ys)

    assocProp :: Semialign f => f Int -> f Int -> f Int -> Property
    assocProp xs ys zs = lhs === fmap assocThese rhs
      where
        rhs = (xs `align` ys) `align` zs
        lhs = xs `align` (ys `align` zs)

    alignToListProp :: Semialign f => f Int -> f Int -> Property
    alignToListProp xs ys =
        toList xs === toListOf (folded . here) xys
        .&&.
        toList xs === mapMaybe justHere (toList xys)
        .&&.
        toList ys === toListOf (folded . there) xys
      where
        xys = align xs ys

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

-------------------------------------------------------------------------------
-- Const is invalid Align with Monoid, we need Idemporent monoid!
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

instance Arbitrary a => Arbitrary (R a) where
    arbitrary = Nest <$> arbitrary
    shrink (Nest xss) = Nest <$> shrink xss

data Index = I1 | I2 | I3 | I4
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Index where
    arbitrary = elements [minBound .. maxBound]
    shrink I1 = []
    shrink I2 = [I1]
    shrink I3 = [I1, I2]
    shrink I4 = [I1, I2, I3]

-------------------------------------------------------------------------------
-- Crosswalk laws
-------------------------------------------------------------------------------

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
-- SearchResult
-------------------------------------------------------------------------------

semigroupLaws
    :: forall a. (Semigroup a, Show a, Eq a, Arbitrary a)
    => String -> a -> TestTree
semigroupLaws name _ = testGroup ("Semigroup: " ++ name)
    [ QC.testProperty "associativity" assocProp
    ]
  where
    assocProp :: a -> a -> a -> Property
    assocProp x y z = (x <> y) <> z === x <> (y <> z)

monoidLaws
    :: forall a. (Monoid a, Show a, Eq a, Arbitrary a)
    => String -> a -> TestTree
monoidLaws name _ = testGroup ("Monoid: " ++ name)
    [ QC.testProperty "associativity" assocProp
    , QC.testProperty "left-identity" idLeftProp
    , QC.testProperty "right-identity" idRightProp
    ]
  where
    assocProp :: a -> a -> a -> Property
    assocProp x y z = (x `mappend` y) `mappend` z === x `mappend` (y `mappend` z)

    idLeftProp :: a -> Property
    idLeftProp x = mappend mempty x === x

    idRightProp :: a -> Property
    idRightProp x = mappend x mempty === x

-- | Either a, or b, or both a and b
--
-- See https://github.com/isomorphism/these/issues/80
data SearchResult a b = Scanned a | Found b | ScannedAndFound a b
  deriving (Eq, Ord, Show)

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

{-
-- almost lawful
instance Monoid a => Monoid (SearchResult a b) where
    mappend = (<>)
    mempty = Scanned mempty
-}
