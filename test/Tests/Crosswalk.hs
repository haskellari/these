{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Crosswalk (crosswalkProps) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.Instances ()
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Identity         (Identity (..))
import Data.Map                      (Map)
import Data.Semigroup                (Semigroup (..))
import Data.Sequence                 (Seq)
import Test.QuickCheck               (Arbitrary (..), Property, (===))
import Test.QuickCheck.Function      (Fun (..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Poly          (A, B, OrdA)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.QuickCheck         (testProperty)

import qualified Data.Vector as V

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
import Data.Typeable (Typeable, typeOf1)
#else
import Data.Typeable (Typeable1, typeOf1)
#endif

import Data.Crosswalk
import Data.Semialign
import Data.These

crosswalkProps :: TestTree
crosswalkProps = testGroup "Crosswalk"
    [ crosswalkLaws (P :: P [])
    , crosswalkLaws (P :: P Maybe)
    , crosswalkLaws (P :: P Identity)
    , crosswalkLaws (P :: P (These Int))
    , crosswalkLaws (P :: P Seq)
    , crosswalkLaws (P :: P V.Vector)
    , crosswalkLaws (P :: P ((,) Int))
#if __GLASGOW_HASKELL__ >= 708
    , crosswalkLaws (P :: P (Compose [] []))
#endif
    ]

-------------------------------------------------------------------------------
-- Crosswalk laws
-------------------------------------------------------------------------------

-- For old GHC to work
data P (a :: * -> *) = P

crosswalkLaws
    :: forall (t :: * -> *).
       ( Typeable1 t, Crosswalk t
       , Eq (t A), Show (t A), Arbitrary (t A)
       , Eq (t B), Show (t B), Arbitrary (t B)
       )
    => P t
    -> TestTree
crosswalkLaws _ = testGroup ("CrossWalk " <> name)
    [ testProperty "crosswalk (const nil) = const nil" firstLaw
    , testProperty "crosswalk f = sequenceL . fmap f" secondLaw
    ]
  where
    name = show (typeOf1 (undefined :: t ()))

    -- f = Map OrdA
    -- a, b = Int
    firstLaw :: t A -> Property
    firstLaw x = lhs === rhs
      where
        lhs = crosswalk (const nil) x
        rhs = const nil x :: Map OrdA (t A)

    secondLaw :: Fun A (Map OrdA B) -> t A -> Property
    secondLaw (Fun _ f) x = lhs === rhs
      where
        lhs = crosswalk f x
        rhs = sequenceL . fmap f $ x
