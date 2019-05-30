{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Align (alignProps) where

import Prelude ()
import Prelude.Compat hiding (unzip, zip, zipWith)

-- import qualified Prelude.Compat as Prelude

import Control.Applicative           (ZipList (..))
import Control.Lens                  (folded, toListOf)
import Control.Monad                 (join)
import Control.Monad.Trans.Instances ()
import Data.Bifunctor                (bimap)
import Data.Bifunctor.Assoc          (assoc)
import Data.Bifunctor.Swap           (swap)
import Data.Foldable                 (toList)
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Product          (Product (..))
import Data.HashMap.Strict           (HashMap)
import Data.IntMap                   (IntMap)
import Data.List.NonEmpty            (NonEmpty)
import Data.Map                      (Map)
import Data.Maybe                    (mapMaybe)
import Data.Proxy                    (Proxy)
import Data.Sequence                 (Seq)
import Data.Tagged                   (Tagged)
import Test.QuickCheck
       (Arbitrary (..), Property, counterexample, (.&&.), (===))
import Test.QuickCheck.Function      (Fun (..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Poly          (A, B, C)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.QuickCheck         (testProperty)

import qualified Data.Tree   as T
import qualified Data.Vector as V

import Data.Semialign
import Data.These
import Data.These.Combinators
import Data.These.Lens

import Tests.Orphans ()

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
import Data.Typeable (Typeable, typeOf1)
#else
import Data.Typeable (Typeable1, typeOf1)
#endif

-------------------------------------------------------------------------------
-- Props
-------------------------------------------------------------------------------

alignProps :: TestTree
alignProps = testGroup "Align"
    [ semialignLaws  (CAll     :: CSemialign [])
    , semialignLaws  (CUnalign :: CSemialign (HashMap String))
    , semialignLaws  (CUnalign :: CSemialign (Map Char))
    , semialignLaws  (CUnalign :: CSemialign IntMap)
    , semialignLaws  (CUnAll   :: CSemialign Maybe)
    , semialignLaws  (CAll     :: CSemialign (Product [] Maybe))
    , semialignLaws  (CUnAll   :: CSemialign (Product Maybe Maybe))
    , semialignLaws  (CAll     :: CSemialign (Compose [] Maybe))
    , semialignLaws  (CAlign   :: CSemialign Seq)
    , semialignLaws  (CAlign   :: CSemialign V.Vector)
    , semialignLaws  (CAlign   :: CSemialign ZipList)
    , semialignLaws  (CZip     :: CSemialign T.Tree)
    , semialignLaws  (CZip     :: CSemialign NonEmpty)
    , semialignLaws  (CZip     :: CSemialign Identity)
    , semialignLaws  (CUnAll   :: CSemialign Proxy)
    , semialignLaws  (CZip     :: CSemialign (Tagged Char))
    ]

-------------------------------------------------------------------------------
-- Align laws
-------------------------------------------------------------------------------

data CSemialign f where
--    CSemialign :: Semialign f                 => CSemialign f
    CAlign     :: Align f                     => CSemialign f
    CUnalign   :: (Align f, Unalign f)        => CSemialign f
    CZip       :: Zip f                       => CSemialign f
    CAll       :: (Align f, Zip f)            => CSemialign f
    CUnAll     :: (Align f, Zip f, Unalign f) => CSemialign f

semialignLaws
    :: forall (f :: * -> *).
       ( Semialign f, Unzip f, Foldable f, Typeable1 f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Eq (f (A, (B, C))), Show (f (A, (B, C)))
       , Eq (f (A, A)), Show (f (A, A))
       , Eq (f (A, B)), Show (f (A, B)), Arbitrary (f (A, B))
       , Eq (f (C, Int)), Show (f (C, Int))
       , Eq (f (These (A, B) C)), Show (f (These (A, B) C))
       , Eq (f (These (A, C) (B, C))), Show (f (These (A, C) (B, C)))
       , Eq (f (These A (A, B))), Show (f (These A (A, B)))
       , Eq (f (These A (These B C))), Show (f (These A (These B C)))
       , Eq (f (These A A)), Show (f (These A A))
       , Eq (f (These A B)), Show (f (These A B)), Arbitrary (f (These A B))
       , Eq (f (These C Int)), Show (f (These C Int))
       )
    => CSemialign f
    -> TestTree
semialignLaws p = testGroup name $ case p of
    -- CSemialign -> semialignLaws'
    CAlign     -> [semialignLaws' p, unzipLaws' p, alignLaws' p]
    CUnalign   -> [semialignLaws' p, unzipLaws' p, unalignLaws' p, alignLaws' p]
    CZip       -> [semialignLaws' p, unzipLaws' p, zipLaws' p]
    CAll       -> [semialignLaws' p, unzipLaws' p, alignLaws' p, zipLaws' p]
    CUnAll     -> [semialignLaws' p, unzipLaws' p, unalignLaws' p, alignLaws' p, zipLaws' p]
  where
    name = show (typeOf1 (undefined :: f ()))

semialignLaws'
    :: forall f proxy. (Semialign f, Foldable f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Eq (f (A, A)), Show (f (A, A))
       , Eq (f (These A A)), Show (f (These A A))
       , Eq (f (These C Int)), Show (f (These C Int))
       , Eq (f (C, Int)), Show (f (C, Int))
       , Eq (f (These A B)), Show (f (These A B))
       , Eq (f (These A (These B C))), Show (f (These A (These B C)))
       , Eq (f (A, B)), Show (f (A, B)), Arbitrary (f (A, B))
       , Eq (f (A, (B, C))), Show (f (A, (B, C)))
       , Eq (f (These A (A, B))), Show (f (These A (A, B)))
       , Eq (f (These (A, B) C)), Show (f (These (A, B) C))
       , Eq (f (These (A, C) (B, C))), Show (f (These (A, C) (B, C)))
       )
    => proxy f -> TestTree
semialignLaws' _ = testGroup "Semialign"
    [ testProperty "idempotency align" idempAlign
    , testProperty "idempotency zip"   idempZip

    , testProperty "commutativity align" swapProp
    , testProperty "commutativity zip"   zipSwapProp

    , testProperty "associativity align" assocProp
    , testProperty "associativity zip"   zipAssocProp

    , testProperty "absoption 1" absorb1Prop
    , testProperty "absoption 2" absorb2Prop

    , testProperty "alignWith" alignWithProp
    , testProperty "zipWith"   zipWithProp

    , testProperty "functoriality align" bimapAlignProp
    , testProperty "functoriality zip" bimapZipProp

    , testProperty "fst-zip" fstZipProp
    , testProperty "snd-zip" sndZipProp
    , testProperty "zip-fst-snd" zipFstSndProp

    , testProperty "alignToList" alignToListProp

    , testProperty "distributivity 1" distr1'Prop
    , testProperty "distributivity 2"  distr2Prop
    , testProperty "distributivity 3'" distr2'Prop

    -- testProperty "distributivity 4"  distr1Prop
    ]
  where
    idempAlign :: f A -> Property
    idempAlign xs = join align xs === fmap (join These) xs

    idempZip :: f A -> Property
    idempZip xs = join zip xs === fmap (join (,)) xs

    bimapAlignProp :: f A -> f B -> Fun A C -> Fun B Int -> Property
    bimapAlignProp xs ys (Fun _ f) (Fun _ g) =
        align (f <$> xs) (g <$> ys) === (bimap f g <$> align xs ys)

    bimapZipProp :: f A -> f B -> Fun A C -> Fun B Int -> Property
    bimapZipProp xs ys (Fun _ f) (Fun _ g) =
        zip (f <$> xs) (g <$> ys) === (bimap f g <$> zip xs ys)

    alignWithProp :: f A -> f B -> Fun (These A B) C -> Property
    alignWithProp xs ys (Fun _ f) =
        alignWith f xs ys === (f <$> align xs ys)

    zipWithProp :: f A -> f B -> Fun (A, B) C -> Property
    zipWithProp xs ys (Fun _ f) =
        zipWith (curry f) xs ys === (f <$> zip xs ys)

    swapProp :: f A -> f B -> Property
    swapProp xs ys = align xs ys === fmap swap (align ys xs)

    assocProp :: f A -> f B -> f C -> Property
    assocProp xs ys zs = lhs === fmap assocThese rhs
      where
        rhs = (xs `align` ys) `align` zs
        lhs = xs `align` (ys `align` zs)

    alignToListProp :: f A -> f B -> Property
    alignToListProp xs ys =
        toList xs === toListOf (folded . here) xys
        .&&.
        toList xs === mapMaybe justHere (toList xys)
        .&&.
        toList ys === toListOf (folded . there) xys
      where
        xys = align xs ys

    fstZipProp :: f A -> Property
    fstZipProp xs = fmap fst (zip xs xs) === xs

    sndZipProp :: f A -> Property
    sndZipProp xs = fmap fst (zip xs xs) === xs

    zipFstSndProp :: f (A, B) -> Property
    zipFstSndProp xs = zip (fmap fst xs) (fmap snd xs) === xs

    zipSwapProp :: f A -> f B -> Property
    zipSwapProp xs ys = zip xs ys === fmap swap (zip ys xs)

    zipAssocProp :: f A -> f B -> f C -> Property
    zipAssocProp xs ys zs = lhs === fmap assoc rhs
      where
        rhs = (xs `zip` ys) `zip` zs
        lhs = xs `zip` (ys `zip` zs)

    absorb1Prop :: f A -> f B -> Property
    absorb1Prop xs ys = fmap fst (zip xs (align xs ys)) === xs

    absorb2Prop :: f A -> f B -> Property
    absorb2Prop xs ys = lhs === rhs where
        lhs = fmap toThis (align xs (zip xs ys))
        rhs = fmap This xs

        toThis (This a)    = This a
        toThis (These a _) = This a
        toThis (That b)    = That b

    -- distr1Prop :: f A -> f B -> f C -> Property
    -- distr1Prop xs ys zs = lhs === rhs where
    --     lhs = distrThesePair <$> align (zip xs ys) zs
    --     rhs = zip (align xs zs) (align ys zs)

    distr1'Prop :: f A -> f B -> f C -> Property
    distr1'Prop xs ys zs = lhs === rhs where
        lhs = align (zip xs ys) zs
        rhs = undistrThesePair <$> zip (align xs zs) (align ys zs)

    distr2Prop :: f A -> f B -> f C -> Property
    distr2Prop xs ys zs = lhs === rhs where
        lhs = distrPairThese <$> zip (align xs ys) zs
        rhs = align (zip xs zs) (zip ys zs)

    distr2'Prop :: f A -> f B -> f C -> Property
    distr2'Prop xs ys zs = lhs === rhs where
        lhs = distrPairThese <$> zip (align xs ys) zs
        rhs = align (zip xs zs) (zip ys zs)

alignLaws'
    :: forall f proxy. (Align f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f (These A B)), Show (f (These A B))
       )
    => proxy f -> TestTree
alignLaws' _ = testGroup "Align"
    [ testProperty "right identity" rightIdentityProp
    , testProperty "left identity" leftIdentityProp
    ]
  where
    rightIdentityProp :: f A -> Property
    rightIdentityProp xs = (xs `align` (nil :: f B)) === fmap This xs

    leftIdentityProp :: Align f => f B -> Property
    leftIdentityProp xs = ((nil :: f A) `align` xs) === fmap That xs

zipLaws'
    :: forall f proxy. (Zip f
       , Eq (f A), Show (f A), Arbitrary (f A)
       )
    => proxy f -> TestTree
zipLaws' _ = testGroup "Zip"
    [ testProperty "right identity" zipRightIdentityProp
    , testProperty "left identity" zipLeftIdentityProp
    ]
  where
    zipRightIdentityProp :: f A -> B -> Property
    zipRightIdentityProp xs y = (fst <$> zip xs (full y)) === xs

    zipLeftIdentityProp :: B -> f A -> Property
    zipLeftIdentityProp x ys = (snd <$> zip (full x) ys) === ys

unalignLaws'
    :: forall f proxy. (Unalign f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Eq (f (These A B)), Show (f (These A B)), Arbitrary (f (These A B))
       )
    => proxy f -> TestTree
unalignLaws' _ = testGroup "Unalign"
    [ testProperty "right inverse" invProp
    , testProperty "left inverse" leftProp
    , testProperty "unalignWith via unalign" unalignWithProp
    , testProperty "unalign via unalignWith" unalignProp
    ]
  where
    unalignWithProp :: f A -> Fun A (These B C) -> Property
    unalignWithProp xs (Fun _ f) = unalignWith f xs === unalign (f <$> xs)

    unalignProp :: f (These A B) -> Property
    unalignProp xs = unalign xs === unalignWith id xs

    invProp :: f (These A B) -> Property
    invProp xs = uncurry align (unalign xs) === xs

    leftProp :: f A -> f B -> Property
    leftProp xs ys = counterexample (show xys) $ unalign xys === (xs, ys) where
        xys = align xs ys


unzipLaws'
    :: forall f proxy. (Unzip f
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       , Eq (f (A, B)), Show (f (A, B)), Arbitrary (f (A, B))
       )
    => proxy f -> TestTree
unzipLaws' _ = testGroup "Unzip"
    [ testProperty "unzip = unzipDefault" def
    , testProperty "unzipWith via unzip" unzipWithProp
    , testProperty "unzip via unzipWith" unzipProp
    , testProperty "right inverse" invProp
    , testProperty "left inverse" leftProp
    ]
  where
    def :: f (A, B) -> Property
    def xs = unzip xs === unzipDefault xs

    unzipWithProp :: f A -> Fun A (B, C) -> Property
    unzipWithProp xs (Fun _ f) = unzipWith f xs === unzip (f <$> xs)

    unzipProp :: f (A, B) -> Property
    unzipProp xs = unzip xs === unzipWith id xs

    invProp :: f (A, B) -> Property
    invProp xs = uncurry zip (unzip xs) === xs

    leftProp :: f A -> Property
    leftProp xs = unzip (zip xs xs) === (xs, xs)
