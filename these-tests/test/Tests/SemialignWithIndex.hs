{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.SemialignWithIndex (alignWithKeyProps) where

import Prelude hiding (zip, repeat)

import Data.Functor.WithIndex    (FunctorWithIndex (imap))
import Test.QuickCheck
       (Arbitrary (..), CoArbitrary, Property, once, (===))
import Test.QuickCheck.Function  (Fun (..), Function, applyFun)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Poly      (A, B, C)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import Control.Applicative (ZipList)
import Data.HashMap.Strict (HashMap)
import Data.IntMap         (IntMap)
import Data.Map            (Map)
import Data.Sequence       (Seq)
import Data.Vector         (Vector)

import Data.Semialign
import Data.Semialign.Indexed
import Data.These

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

alignWithKeyProps :: TestTree
alignWithKeyProps = testGroup "AlignWithIndex"
    [ testProperty "example" $ once exampleI
    , semialignIndexedLaws (P :: P []) -- cannot test irepeat, because it's infinite.
    , semialignIndexedLaws (P :: P ZipList)
    , semialignIndexedLaws (P :: P IntMap)
    , semialignIndexedLaws (P :: P (Map Int))
    , semialignIndexedLaws (P :: P (HashMap Char))
    , semialignIndexedLaws (P :: P Seq)
    , semialignIndexedLaws (P :: P Vector)
    , repeatIndexedLaws (P :: P Maybe)
    ]
  where
    exampleI = ialignWith (,) "foo" "quux" === exampleV

    exampleV =
        [ (0, These 'f' 'q')
        , (1, These 'o' 'u')
        , (2, These 'o' 'u')
        , (3, That 'x')
        ]

-------------------------------------------------------------------------------
-- Laws
-------------------------------------------------------------------------------

data P (f :: * -> *) = P

repeatIndexedLaws
    :: forall f i. (RepeatWithIndex i f, Typeable1 f
       , Function i, CoArbitrary i, Show i
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       )
    => P f
    -> TestTree
repeatIndexedLaws p = testGroup name $
    semialignIndexedLaws' p ++
    [ testProperty "irepeat definition" irepeatDef
    ]
  where
    name = show (typeOf1 (undefined :: f ()))

    irepeatDef :: Fun i A -> Property
    irepeatDef f' = irepeat f === imap (\i g -> g i) (repeat f :: f (i -> A)) where
        f = applyFun f'

semialignIndexedLaws
    :: forall f i. (ZipWithIndex i f, Typeable1 f
       , Function i, CoArbitrary i, Show i
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       )
    => P f
    -> TestTree
semialignIndexedLaws p = testGroup name $ semialignIndexedLaws' p where
    name = show (typeOf1 (undefined :: f ()))


semialignIndexedLaws'
    :: forall f i. (ZipWithIndex i f, Typeable1 f
       , Function i, CoArbitrary i, Show i
       , Eq (f A), Show (f A), Arbitrary (f A)
       , Eq (f B), Show (f B), Arbitrary (f B)
       , Eq (f C), Show (f C), Arbitrary (f C)
       )
    => P f
    -> [TestTree]
semialignIndexedLaws' _ =
    [ testProperty "ialignWith definition" ialignDef
    , testProperty "izipWith definition" izipDef
    ]
  where
    ialignDef :: Fun (i, These A B) C -> f A -> f B -> Property
    ialignDef f' xs ys = ialignWith f xs ys === imap f (align xs ys) where
        f i ab = applyFun f' (i, ab)

    izipDef :: Fun (i, A, B) C -> f A -> f B -> Property
    izipDef f' xs ys = izipWith f xs ys === imap (uncurry . f) (zip xs ys) where
        f i a b = applyFun f' (i, a, b)
