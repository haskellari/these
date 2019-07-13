{-# LANGUAGE Trustworthy #-}
-- | Zipping and aligning of functors with non-uniform shapes.
module Data.Semialign (
    Semialign (..),
    Align (..),
    Unalign (..),
    Zip (..),
    Unzip (..),
    unzipDefault,
    -- * Specialized aligns
    malign, salign, padZip, padZipWith,
    lpadZip, lpadZipWith,
    rpadZip, rpadZipWith,
    alignVectorWith,
    -- * Diffing/patching
    diff, diffNoEq, patch
    ) where

import Data.Semialign.Internal
