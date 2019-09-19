{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Trans.Chronicle
--
-- The 'ChronicleT' monad, a hybrid error/writer monad that allows
-- both accumulating outputs and aborting computation with a final
-- output.
-----------------------------------------------------------------------------
module Control.Monad.Chronicle (
    -- * Type class for Chronicle-style monads
      MonadChronicle(..)
    -- * The ChronicleT monad transformer
    , Chronicle, runChronicle, ChronicleT(..)
    ) where

import Control.Monad.Chronicle.Class
import Control.Monad.Trans.Chronicle (Chronicle, ChronicleT (..), runChronicle)
