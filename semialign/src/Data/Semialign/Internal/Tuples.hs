{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy #-}
module Data.Semialign.Internal.Tuples
  ( SBPair (..)
  , LBPair (..)
  , Solo (..)
  , getSolo
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Biapplicative (Biapplicative (..))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..))
#endif

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (noinline)
#elif MIN_VERSION_ghc_prim(0,5,1)
import GHC.Magic (noinline)
#endif

-- A copy of (,) with a stricter bimap.
newtype SBPair a b = SBPair { unSBPair :: (a, b) }

instance Bifunctor SBPair where
  bimap f g (SBPair (a, b)) = SBPair (f a, g b)

instance Biapplicative SBPair where
  bipure a b = SBPair (a, b)
  biliftA2 f g (SBPair (a, b)) (SBPair (c, d)) =
    SBPair (f a c, g b d)

-- A copy of (,) with a lazier biliftA2
newtype LBPair a b = LBPair { unLBPair :: (a, b) }

instance Bifunctor LBPair where
  bimap = bimapLB

bimapLB :: (a -> c) -> (b -> d) -> LBPair a b -> LBPair c d
bimapLB f g (LBPair ab) = LBPair (f a, g b)
    where
      -- This stuff can be really touchy, so we're extra careful.
      -- We want a and b to be actual selector thunks. If their
      -- definitions inline, then they won't be. Why do we say
      -- noinline ab? That may be a bit belt-and-suspenders, but
      -- I've been bitten in the past. The concern is that GHC
      -- could see
      --
      --   bimapLB f g p@(LBPair (e1, e2))
      --
      -- and decide to do something like
      --
      --   let (a, _) = p
      --   in LBPair (f a, g e2)
      --
      -- I don't remember the details, but something similar happened
      -- when defining Data.List.transpose, so I'll just be careful
      -- until it's proven unnecessary.
      {-# NOINLINE a #-}
      {-# NOINLINE b #-}
      (a, b) = noinline ab
{-# NOINLINE [1] bimapLB #-}

-- Optimize when we can, being sure to expand both sides.
-- Hopefully these rules can't break the selector thunks.
{-# RULES
"bimap/known" forall f g a b. bimapLB f g (LBPair (a, b)) = LBPair (f a, g b)
 #-}

instance Biapplicative LBPair where
  bipure a b = LBPair (a, b)
  biliftA2 = biliftA2LB

biliftA2LB :: (a -> c -> e) -> (b -> d -> f) -> LBPair a b -> LBPair c d -> LBPair e f
biliftA2LB f g (LBPair ab) (LBPair cd) = LBPair (f a c, g b d)
    where
      {-# NOINLINE a #-}
      {-# NOINLINE b #-}
      {-# NOINLINE c #-}
      {-# NOINLINE d #-}
      (a, b) = noinline ab
      (c, d) = noinline cd
{-# NOINLINE [1] biliftA2LB #-}

biliftA2LBkl :: (a -> c -> e) -> (b -> d -> f) -> a -> b -> LBPair c d -> LBPair e f
biliftA2LBkl f g a b (LBPair cd) = LBPair (f a c, g b d)
    where
      {-# NOINLINE c #-}
      {-# NOINLINE d #-}
      (c, d) = noinline cd
{-# NOINLINE [1] biliftA2LBkl #-}

biliftA2LBkr :: (a -> c -> e) -> (b -> d -> f) -> LBPair a b -> c -> d -> LBPair e f
biliftA2LBkr f g (LBPair ab) c d = LBPair (f a c, g b d)
    where
      {-# NOINLINE a #-}
      {-# NOINLINE b #-}
      (a, b) = noinline ab
{-# NOINLINE [1] biliftA2LBkr #-}

{-# RULES
"biliftA2/knownl" forall f g a b cd. biliftA2LB f g (LBPair (a, b)) cd
  = biliftA2LBkl f g a b cd
"biliftA2/knownlr" forall f g a b c d. biliftA2LBkl f g a b (LBPair (c, d))
  = LBPair (f a c, g b d)
"biliftA2/knownr" forall f g ab c d. biliftA2LB f g ab (LBPair (c, d))
  = biliftA2LBkr f g ab c d
"biliftA2/knownrl" forall f g a b c d. biliftA2LBkr f g (LBPair (a, b)) c d
  = LBPair (f a c, g b d)
 #-}

-- ----------
-- Compat stuff.

-- As of GHC 9.0, Solo is not exported from base (it's stuck in ghc-prim).
-- Hopefully this will be sorted by 9.2, and it will definitely be sorted by
-- 9.4. I'd rather avoid an unconditional dependency on ghc-prim, especially
-- when we just need two instances and one of them is derived.
data Solo a = Solo { getSolo :: a }
  deriving Functor

instance Applicative Solo where
  pure = Solo
  Solo f <*> Solo a = Solo (f a)

#if !MIN_VERSION_ghc_prim(0,5,1)
{-# NOINLINE noinline #-}
noinline :: a -> a
noinline a = a
#endif
