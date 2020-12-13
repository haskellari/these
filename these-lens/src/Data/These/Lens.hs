{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.These.Lens (
    -- * Traversals
    here, there,

    -- * Prisms
    _This, _That, _These,
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Prelude             (Either (..), flip, uncurry, ($), (.))

import Control.Lens (Prism', Traversal, prism)
import Data.These

#if !MIN_VERSION_lens(5,0,0)
import Control.Lens           (Each (..), Swapped (..), iso)
import Data.These.Combinators (swapThese)
#endif

-- $setup
-- >>> import Data.These
-- >>> import Control.Lens
-- >>> import Prelude (show)

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

-- | A 'Control.Lens.Traversal' of the first half of a 'These', suitable for use with "Control.Lens".
--
-- >>> over here show (That 1)
-- That 1
--
-- >>> over here show (These 'a' 2)
-- These "'a'" 2
--
here :: Traversal (These a c) (These b c) a b
here f (This x) = This <$> f x
here f (These x y) = flip These y <$> f x
here _ (That x) = pure (That x)

-- | A 'Control.Lens.Traversal' of the second half of a 'These', suitable for use with "Control.Lens".
--
-- @
-- 'there' :: 'Control.Lens.Traversal' ('These' t b) ('These' t b) a b
-- @
--
-- >>> over there show (That 1)
-- That "1"
--
-- >>> over there show (These 'a' 2)
-- These 'a' "2"
--
there :: Traversal (These c a) (These c b) a b
there _ (This x) = pure (This x)
there f (These x y) = These x <$> f y
there f (That x) = That <$> f x

-------------------------------------------------------------------------------
-- Prisms
-------------------------------------------------------------------------------

-- | A 'Control.Lens.Prism'' selecting the 'This' constructor.
--
-- /Note:/ cannot change type.
_This :: Prism' (These a b) a
_This = prism This (these Right (Left . That) (\x y -> Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'That' constructor.
--
-- /Note:/ cannot change type.
_That :: Prism' (These a b) b
_That = prism That (these (Left . This) Right (\x y -> Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'These' constructor. 'These' names are ridiculous!
--
-- /Note:/ cannot change type.
_These :: Prism' (These a b) (a, b)
_These = prism (uncurry These) (these (Left . This) (Left . That) (\x y -> Right (x, y)))

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

#if !MIN_VERSION_lens(5,0,0)
instance Swapped These where
    swapped = iso swapThese swapThese

-- | @since 1.0.1
instance (a ~ a', b ~ b') => Each (These a a') (These b b') a b where
    each f (This a)    = This <$> f a
    each f (That b)    = That <$> f b
    each f (These a b) = These <$> f a <*> f b
#endif
