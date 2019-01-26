{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}

#if MIN_VERSION_base(4,9,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#if MIN_VERSION_transformers(0,5,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#if MIN_VERSION_transformers_compat(0,5,0) && !MIN_VERSION_transformers(0,4,0)
#define LIFTED_FUNCTOR_CLASSES 1
#endif
#endif
#endif

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
module Data.Functor.These (
    These1 (..),
    ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
       (FromJSON (..), FromJSON1 (..), ToJSON (..), ToJSON1 (..), (.=))
import Data.Data            (Data)
import Data.Functor.Classes
       (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..), compare1, eq1, readsPrec1,
       showsPrec1)
import Data.Typeable        (Typeable)
import GHC.Generics         (Generic, Generic1)
import Test.QuickCheck
       (Arbitrary (..), Arbitrary1 (..), arbitrary1, liftShrink2, oneof,
       shrink1)

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData (..), NFData1 (..), rnf1)
#endif

import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Encoding as Aeson (pair)
import qualified Data.HashMap.Strict as HM

data These1 f g a
    = This1 (f a)
    | That1 (g a)
    | These1 (f a) (g a)
  deriving (Functor, Foldable, Traversable, Generic
#if __GLASGOW_HASKELL__ >= 706
    , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 708
    , Typeable, Data
#endif
    )

-------------------------------------------------------------------------------
-- Eq1
-------------------------------------------------------------------------------

instance (Eq1 f, Eq1 g) => Eq1 (These1 f g) where
#ifdef LIFTED_FUNCTOR_CLASSES
    liftEq eq (This1 f)    (This1 f')     = liftEq eq f f'
    liftEq eq (That1 g)    (That1 g')     = liftEq eq g g'
    liftEq eq (These1 f g) (These1 f' g') = liftEq eq f f' && liftEq eq g g'

    liftEq _ This1  {} _ = False
    liftEq _ That1  {} _ = False
    liftEq _ These1 {} _ = False
#else
    eq1 (This1 f)    (This1 f')     = eq1 f f'
    eq1 (That1 g)    (That1 g')     = eq1 g g'
    eq1 (These1 f g) (These1 f' g') = eq1 f f' && eq1 g g'

    eq1 This1  {} _ = False
    eq1 That1  {} _ = False
    eq1 These1 {} _ = False
#endif

-------------------------------------------------------------------------------
-- Ord1
-------------------------------------------------------------------------------

instance (Ord1 f, Ord1 g) => Ord1 (These1 f g) where
#ifdef LIFTED_FUNCTOR_CLASSES
    liftCompare  cmp (This1 f) (This1 f') = liftCompare cmp f f'
    liftCompare _cmp (This1 _) _          = LT
    liftCompare _cmp _         (This1 _)  = GT

    liftCompare  cmp (That1 g) (That1 g') = liftCompare cmp g g'
    liftCompare _cmp (That1 _) _          = LT
    liftCompare _cmp _         (That1 _)  = GT

    liftCompare  cmp (These1 f g) (These1 f' g') =
        liftCompare cmp f f' `mappend` liftCompare cmp g g'
#else
    compare1 (This1 f) (This1 f') = compare1 f f'
    compare1 (This1 _) _          = LT
    compare1 _         (This1 _)  = GT

    compare1 (That1 g) (That1 g') = compare1 g g'
    compare1 (That1 _) _          = LT
    compare1 _         (That1 _)  = GT

    compare1  (These1 f g) (These1 f' g') =
        compare1 f f' `mappend` compare1 g g'
#endif


-------------------------------------------------------------------------------
-- Show1
-------------------------------------------------------------------------------

instance (Show1 f, Show1 g) => Show1 (These1 f g) where
#ifdef LIFTED_FUNCTOR_CLASSES
    liftShowsPrec sp sl d (This1 f) = showParen (d > 10)
        $ showString "This1 "
        . liftShowsPrec sp sl 11 f
    liftShowsPrec sp sl d (That1 g) = showParen (d > 10)
        $ showString "That1 "
        . liftShowsPrec sp sl 11 g
    liftShowsPrec sp sl d (These1 f g) = showParen (d > 10)
        $ showString "These1 "
        . liftShowsPrec sp sl 11 f
        . showChar ' '
        . liftShowsPrec sp sl 11 g
#else
    showsPrec1 d (This1 f) = showParen (d > 10)
        $ showString "This1 "
        . showsPrec1 11 f
    showsPrec1 d (That1 g) = showParen (d > 10)
        $ showString "That1 "
        . showsPrec1 11 g
    showsPrec1 d (These1 f g) = showParen (d > 10)
        $ showString "These1 "
        . showsPrec1 11 f
        . showChar ' '
        . showsPrec1 11 g
#endif

-------------------------------------------------------------------------------
-- Read1
-------------------------------------------------------------------------------

instance (Read1 f, Read1 g) => Read1 (These1 f g) where
#ifdef LIFTED_FUNCTOR_CLASSES
    liftReadsPrec rp rl d = readParen (d > 10) $ \s0 -> do
        (t, s1) <- lex s0
        case t of
            "This1" -> do
                (x, s2) <- liftReadsPrec rp rl 11 s1
                return (This1 x, s2)
            "That1" -> do
                (y, s2) <- liftReadsPrec rp rl 11 s1
                return (That1 y, s2)
            "These1" -> do
                (x, s2) <- liftReadsPrec rp rl 11 s1
                (y, s3) <- liftReadsPrec rp rl 11 s2
                return (These1 x y, s3)
            _ -> []
#else
    readsPrec1 d = readParen (d > 10) $ \s0 -> do
        (t, s1) <- lex s0
        case t of
            "This1" -> do
                (x, s2) <- readsPrec1 11 s1
                return (This1 x, s2)
            "That1" -> do
                (y, s2) <- readsPrec1 11 s1
                return (That1 y, s2)
            "These1" -> do
                (x, s2) <- readsPrec1 11 s1
                (y, s3) <- readsPrec1 11 s2
                return (These1 x y, s3)
            _ -> []
#endif

-------------------------------------------------------------------------------
-- Eq, Ord, Show, Read
-------------------------------------------------------------------------------

instance (Eq1 f, Eq1 g, Eq a) => Eq (These1 f g a) where (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (These1 f g a) where compare = compare1
instance (Show1 f, Show1 g, Show a) => Show (These1 f g a) where showsPrec = showsPrec1
instance (Read1 f, Read1 g, Read a) => Read (These1 f g a) where readsPrec = readsPrec1

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

#if MIN_VERSION_deepseq(1,4,3)
-- | This instance is available only with @deepseq >= 1.4.3.0@
instance (NFData1 f, NFData1 g) => NFData1 (These1 f g) where
    liftRnf r (This1 x)    = liftRnf r x
    liftRnf r (That1 y)    = liftRnf r y
    liftRnf r (These1 x y) = liftRnf r x `seq` liftRnf r y

-- | This instance is available only with @deepseq >= 1.4.3.0@
instance (NFData1 f, NFData1 g, NFData a) => NFData (These1 f g a) where
    rnf = rnf1
#endif

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (These1 f g) where
    liftToJSON tx tl (This1 a)    = Aeson.object [ "This" .= liftToJSON tx tl a ]
    liftToJSON tx tl (That1 b)    = Aeson.object [ "That" .= liftToJSON tx tl b ]
    liftToJSON tx tl (These1 a b) = Aeson.object [ "This" .= liftToJSON tx tl a, "That" .= liftToJSON tx tl b ]

    liftToEncoding tx tl (This1 a)    = Aeson.pairs $ Aeson.pair "This" (liftToEncoding tx tl a)
    liftToEncoding tx tl (That1 b)    = Aeson.pairs $ Aeson.pair "That" (liftToEncoding tx tl b)
    liftToEncoding tx tl (These1 a b) = Aeson.pairs $
        Aeson.pair "This" (liftToEncoding tx tl a) `mappend`
        Aeson.pair "That" (liftToEncoding tx tl b)

instance (FromJSON1 f, FromJSON1 g) => FromJSON1 (These1 f g) where
    liftParseJSON px pl = Aeson.withObject "These1" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These1 <$> liftParseJSON px pl a <*> liftParseJSON px pl b
        p [("That", b), ("This", a)] = These1 <$> liftParseJSON px pl a <*> liftParseJSON px pl b
        p [("This", a)] = This1 <$> liftParseJSON px pl a
        p [("That", b)] = That1 <$> liftParseJSON px pl b
        p _  = fail "Expected object with 'This' and 'That' keys only"

instance (ToJSON1 f, ToJSON1 g, ToJSON a) => ToJSON (These1 f g a) where
    toJSON = Aeson.toJSON1
    toEncoding = Aeson.toEncoding1

instance (FromJSON1 f, FromJSON1 g, FromJSON a) => FromJSON (These1 f g a) where
    parseJSON = Aeson.parseJSON1

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (These1 f g) where
    liftArbitrary arb = oneof
        [ This1 <$> liftArbitrary arb
        , That1 <$> liftArbitrary arb
        , These1 <$> liftArbitrary arb <*> liftArbitrary arb
        ]

    liftShrink shr (This1 x) = This1 <$> liftShrink shr x
    liftShrink shr (That1 y) = That1 <$> liftShrink shr y
    liftShrink shr (These1 x y) =
        [ This1 x, That1 y ] ++
        [ These1 x' y'
        | (x', y') <- liftShrink2 (liftShrink shr) (liftShrink shr) (x, y)
        ]

instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (These1 f g a) where
    arbitrary = arbitrary1
    shrink    = shrink1
