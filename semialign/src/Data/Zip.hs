{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy   #-}
-- | Zipping and unzipping of functors with non-uniform shapes.
--
module Data.Zip (
    Semialign (..),
    Zip (..),
    Unzip (..),
    unzipDefault,
    Zippy (..),
    ) where

import Prelude ()
import Prelude.Compat hiding (zipWith)

import Data.Semigroup (Semigroup (..))

import Data.Semialign.Internal

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Apply (Apply (..))
#endif

-------------------------------------------------------------------------------
-- Zippy
-------------------------------------------------------------------------------

newtype Zippy f a = Zippy { getZippy :: f a }
  deriving (Eq, Ord, Show, Read, Functor)

instance (Semialign f, Semigroup a) => Semigroup (Zippy f a) where
    Zippy x <> Zippy y = Zippy $ zipWith (<>) x y

instance (Zip f, Monoid a) => Monoid (Zippy f a) where
    mempty                      = Zippy $ full mempty
    mappend (Zippy x) (Zippy y) = Zippy $ zipWith mappend x y

#ifdef MIN_VERSION_semigroupoids
instance Semialign f => Apply (Zippy f) where
    Zippy f <.> Zippy x = Zippy $ zipWith ($) f x
#endif

instance Zip f => Applicative (Zippy f) where
    pure  = Zippy . full
#ifdef MIN_VERSION_semigroupoids
    (<*>) = (<.>)
#else
    Zippy f <*> Zippy x = Zippy $ zipWith ($) f x
#endif
