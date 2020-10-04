{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.These.Optics (
    -- * Affine traversals
    here, there,

    -- * Prisms
    _This, _That, _These,
    ) where

import Data.These
import Data.These.Combinators (swapThese)
import Optics.Core
       (AffineTraversal, Each (..), Prism', Swapped (..), atraversalVL, iso,
       itraversalVL, prism)

-- $setup
-- >>> import Optics.Core

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

-- | An 'AffineTraversal' of the first half of a 'These'.
--
-- >>> over here show (That 1)
-- That 1
--
-- >>> over here show (These 'a' 2)
-- These "'a'" 2
--
here :: AffineTraversal (These a c) (These b c) a b
here = atraversalVL here' where
    here' _     f (This x)    = This <$> f x
    here' _     f (These x y) = flip These y <$> f x
    here' point _ (That x)    = point (That x)

-- | An 'AffineTraversal' of the second half of a 'These'.
--
-- >>> over there show (That 1)
-- That "1"
--
-- >>> over there show (These 'a' 2)
-- These 'a' "2"
--
there :: AffineTraversal (These c a) (These c b) a b
there = atraversalVL there' where
    there' point _ (This x)    = point (This x)
    there' _     f (These x y) = These x <$> f y
    there' _     f (That x)    = That <$> f x

-------------------------------------------------------------------------------
-- Prisms
-------------------------------------------------------------------------------

-- | A 'Prism'' selecting the 'This' constructor.
--
-- /Note:/ cannot change type.
_This :: Prism' (These a b) a
_This = prism This (these Right (Left . That) (\x y -> Left $ These x y))

-- | A 'Prism'' selecting the 'That' constructor.
--
-- /Note:/ cannot change type.
_That :: Prism' (These a b) b
_That = prism That (these (Left . This) Right (\x y -> Left $ These x y))

-- | A 'Prism'' selecting the 'These' constructor. 'These' names are ridiculous!
--
-- /Note:/ cannot change type.
_These :: Prism' (These a b) (a, b)
_These = prism (uncurry These) (these (Left . This) (Left . That) (\x y -> Right (x, y)))

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Swapped These where
    swapped = iso swapThese swapThese

-- | @since 1.0.1
instance (a ~ a', b ~ b') => Each (Either () ()) (These a a') (These b b') a b where
    each = itraversalVL aux where
        aux f (This a)    = This <$> f (Left ()) a
        aux f (That b)    = That <$> f (Right ()) b
        aux f (These a b) = These <$> f (Left ()) a <*> f (Right ()) b
