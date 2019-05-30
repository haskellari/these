{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Orphans where

#if !(MIN_VERSION_base(4,7,0))
import Data.Typeable.Internal
import Data.Functor.Compose (Compose)
import Data.Functor.Product (Product)

instance (Typeable1 f, Typeable1 g) => Typeable1 (Product f g) where
    typeOf1 _ = mkTyConApp
        (mkTyCon3 "transformers" "Data.Functor.Product" "Product")
        [typeOf1 (undefined :: f ()), typeOf1 (undefined :: f ())]

instance (Typeable1 f, Typeable1 g) => Typeable1 (Compose f g) where
    typeOf1 _ = mkTyConApp
        (mkTyCon3 "transformers" "Data.Functor.Compose" "Compose")
        [typeOf1 (undefined :: f ()), typeOf1 (undefined :: f ())]
#endif
