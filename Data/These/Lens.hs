module Data.These.Lens (
    -- * Traversals
    here, there,

    -- * Prisms
    _This, _That, _These,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Lens                 (Prism', prism)
import Data.These

-- $setup
-- >>> import Control.Lens

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

-- | A 'Control.Lens.Traversal' of the first half of a 'These', suitable for use with "Control.Lens".
--
-- @
-- 'here' :: 'Control.Lens.Traversal' ('These' a t) ('These' b t) a b
-- @
--
-- >>> over here show (That 1)
-- That 1
--
-- >>> over here show (These 'a' 2)
-- These "'a'" 2
--
here :: (Applicative f) => (a -> f b) -> These a t -> f (These b t)
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
there :: (Applicative f) => (a -> f b) -> These t a -> f (These t b)
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
