{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Crosswalk (crosswalkProps) where

import Control.Applicative           (Const)
import Control.Monad.Trans.Instances ()
import Control.Monad.Trans.Maybe     (MaybeT)
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Sum              (Sum)
import Data.Functor.These            (These1)
import Data.List.NonEmpty            (NonEmpty)
import Data.Map                      (Map)
import Data.Proxy                    (Proxy)
import Data.Semigroup                (Semigroup (..))
import Data.Sequence                 (Seq)
import Data.Typeable                 (Typeable, typeOf1)
import Test.QuickCheck               (Arbitrary (..), Property, (===))
import Test.QuickCheck.Function      (Fun (..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Poly          (A, B, OrdA)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.QuickCheck         (testProperty)

import qualified Data.Vector as V

import Data.Crosswalk
import Data.Semialign
import Data.These

import Tests.Orphans ()

crosswalkProps :: TestTree
crosswalkProps = testGroup "Crosswalk"
    [ crosswalkLaws (P :: P Identity)
    , crosswalkLaws (P :: P Maybe)
    , crosswalkLaws (P :: P [])
    , crosswalkLaws (P :: P NonEmpty)
    , crosswalkLaws (P :: P Seq)
    , crosswalkLaws (P :: P V.Vector)
    , crosswalkLaws (P :: P (Either Int))
    , crosswalkLaws (P :: P (These Int))
    , crosswalkLaws (P :: P ((,) Int))
    , crosswalkLaws (P :: P Proxy)
    , crosswalkLaws (P :: P (Const Int))
    , crosswalkLaws (P :: P (Sum [] []))
    , crosswalkLaws (P :: P (These1 [] []))
    , crosswalkLaws (P :: P (Compose [] []))
    , crosswalkLaws (P :: P (MaybeT []))
    ]

-------------------------------------------------------------------------------
-- Crosswalk laws
-------------------------------------------------------------------------------

-- For old GHC to work
data P (a :: * -> *) = P

crosswalkLaws
    :: forall (t :: * -> *).
       ( Typeable t, Crosswalk t
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
