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
    ) where

import Data.Semialign.Internal
