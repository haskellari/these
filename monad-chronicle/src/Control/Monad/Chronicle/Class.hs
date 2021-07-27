{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- for the ErrorT instances
-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Chronicle.Class
--
-- Hybrid error/writer monad class that allows both accumulating outputs and
-- aborting computation with a final output.
--
-- The expected use case is for computations with a notion of fatal vs.
-- non-fatal errors.
--
-----------------------------------------------------------------------------
module Control.Monad.Chronicle.Class (
    MonadChronicle(..),
    ) where

import           Control.Applicative
import           Control.Monad.Trans.Chronicle (ChronicleT)
import qualified Control.Monad.Trans.Chronicle as Ch
import           Data.These
import           Data.These.Combinators

import Control.Monad.Trans.Except        as Except
import Control.Monad.Trans.Identity      as Identity
import Control.Monad.Trans.Maybe         as Maybe
import Control.Monad.Trans.RWS.Lazy      as LazyRWS
import Control.Monad.Trans.RWS.Strict    as StrictRWS
import Control.Monad.Trans.Reader        as Reader
import Control.Monad.Trans.State.Lazy    as LazyState
import Control.Monad.Trans.State.Strict  as StrictState
import Control.Monad.Trans.Writer.Lazy   as LazyWriter
import Control.Monad.Trans.Writer.Strict as StrictWriter

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Error as Error
#endif

import Control.Monad             (liftM)
import Control.Monad.Trans.Class (lift)
import Data.Default.Class
import Data.Semigroup
import Prelude

class (Monad m) => MonadChronicle c m | m -> c where
    -- | @'dictate' c@ is an action that records the output @c@.
    --
    --   Equivalent to 'tell' for the 'Writer' monad.
    dictate :: c -> m ()

    -- | @'disclose' c@ is an action that records the output @c@ and returns a
    --   @'Default'@ value.
    --
    --   This is a convenience function for reporting non-fatal errors in one
    --   branch a @case@, or similar scenarios when there is no meaningful
    --   result but a placeholder of sorts is needed in order to continue.
    disclose :: (Default a) => c -> m a
    disclose c = dictate c >> return def

    -- | @'confess' c@ is an action that ends with a final record @c@.
    --
    --   Equivalent to 'throwError' for the 'Error' monad.
    confess :: c -> m a

    -- | @'memento' m@ is an action that executes the action @m@, returning either
    --   its record if it ended with 'confess', or its final value otherwise, with
    --   any record added to the current record.
    --
    --   Similar to 'catchError' in the 'Error' monad, but with a notion of
    --   non-fatal errors (which are accumulated) vs. fatal errors (which are caught
    --   without accumulating).
    memento :: m a -> m (Either c a)

    -- | @'absolve' x m@ is an action that executes the action @m@ and discards any
    --   record it had. The default value @x@ will be used if @m@ ended via
    --   'confess'.
    absolve :: a -> m a -> m a

    -- | @'condemn' m@ is an action that executes the action @m@ and keeps its value
    --   only if it had no record. Otherwise, the value (if any) will be discarded
    --   and only the record kept.
    --
    --   This can be seen as converting non-fatal errors into fatal ones.
    condemn :: m a -> m a

    -- | @'retcon' f m@ is an action that executes the action @m@ and applies the
    --   function @f@ to its output, leaving the return value unchanged.
    --
    --   Equivalent to 'censor' for the 'Writer' monad.
    retcon :: (c -> c) -> m a -> m a

    -- | @'chronicle' m@ lifts a plain @'These' c a@ value into a 'MonadChronicle' instance.
    chronicle :: These c a -> m a


instance (Semigroup c) => MonadChronicle c (These c) where
    dictate c = These c ()
    confess c = This c
    memento (This c) = That (Left c)
    memento m = mapThere Right m
    absolve x (This _) = That x
    absolve _ (That x) = That x
    absolve _ (These _ x) = That x
    condemn (These c _) = This c
    condemn m = m
    retcon = mapHere
    chronicle = id

instance (Semigroup c, Monad m) => MonadChronicle c (ChronicleT c m) where
    dictate = Ch.dictate
    confess = Ch.confess
    memento = Ch.memento
    absolve = Ch.absolve
    condemn = Ch.condemn
    retcon = Ch.retcon
    chronicle = Ch.ChronicleT . return

instance (MonadChronicle c m) => MonadChronicle c (IdentityT m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (IdentityT m) = lift $ memento m
    absolve x (IdentityT m) = lift $ absolve x m
    condemn (IdentityT m) = lift $ condemn m
    retcon f (IdentityT m) = lift $ retcon f m
    chronicle = lift . chronicle

instance (MonadChronicle c m) => MonadChronicle c (MaybeT m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (MaybeT m) = MaybeT $ either (Just . Left) (Right <$>) `liftM` memento m
    absolve x (MaybeT m) = MaybeT $ absolve (Just x) m
    condemn (MaybeT m) = MaybeT $ condemn m
    retcon f (MaybeT m) = MaybeT $ retcon f m
    chronicle = lift . chronicle

#if !(MIN_VERSION_transformers(0,6,0))
instance (Error e, MonadChronicle c m) => MonadChronicle c (ErrorT e m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (ErrorT m) = ErrorT $ either (Right . Left) (Right <$>) `liftM` memento m
    absolve x (ErrorT m) = ErrorT $ absolve (Right x) m
    condemn (ErrorT m) = ErrorT $ condemn m
    retcon f (ErrorT m) = ErrorT $ retcon f m
    chronicle = lift . chronicle
#endif

instance (MonadChronicle c m) => MonadChronicle c (ExceptT e m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (ExceptT m) = ExceptT $ either (Right . Left) (Right <$>) `liftM` memento m
    absolve x (ExceptT m) = ExceptT $ absolve (Right x) m
    condemn (ExceptT m) = ExceptT $ condemn m
    retcon f (ExceptT m) = ExceptT $ retcon f m
    chronicle = lift . chronicle

instance (MonadChronicle c m) => MonadChronicle c (ReaderT r m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (ReaderT m) = ReaderT $ memento . m
    absolve x (ReaderT m) = ReaderT $ absolve x . m
    condemn (ReaderT m) = ReaderT $ condemn . m
    retcon f (ReaderT m) = ReaderT $ retcon f . m
    chronicle = lift . chronicle

instance (MonadChronicle c m) => MonadChronicle c (LazyState.StateT s m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (LazyState.StateT m) = LazyState.StateT $ \s -> do
        either (\c -> (Left c, s)) (\(a, s') -> (Right a, s')) `liftM` memento (m s)
    absolve x (LazyState.StateT m) = LazyState.StateT $ \s -> absolve (x, s) $ m s
    condemn (LazyState.StateT m) = LazyState.StateT $ condemn . m
    retcon f (LazyState.StateT m) = LazyState.StateT $ retcon f . m
    chronicle = lift . chronicle

instance (MonadChronicle c m) => MonadChronicle c (StrictState.StateT s m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (StrictState.StateT m) = StrictState.StateT $ \s -> do
        either (\c -> (Left c, s)) (\(a, s') -> (Right a, s')) `liftM` memento (m s)
    absolve x (StrictState.StateT m) = StrictState.StateT $ \s -> absolve (x, s) $ m s
    condemn (StrictState.StateT m) = StrictState.StateT $ condemn . m
    retcon f (StrictState.StateT m) = StrictState.StateT $ retcon f . m
    chronicle = lift . chronicle

instance (Monoid w, MonadChronicle c m) => MonadChronicle c (LazyWriter.WriterT w m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (LazyWriter.WriterT m) = LazyWriter.WriterT $
        either (\c -> (Left c, mempty)) (\(a, w) -> (Right a, w)) `liftM` memento m
    absolve x (LazyWriter.WriterT m) = LazyWriter.WriterT $ absolve (x, mempty) m
    condemn (LazyWriter.WriterT m) = LazyWriter.WriterT $ condemn m
    retcon f (LazyWriter.WriterT m) = LazyWriter.WriterT $ retcon f m
    chronicle = lift . chronicle

instance (Monoid w, MonadChronicle c m) => MonadChronicle c (StrictWriter.WriterT w m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (StrictWriter.WriterT m) = StrictWriter.WriterT $
        either (\c -> (Left c, mempty)) (\(a, w) -> (Right a, w)) `liftM` memento m
    absolve x (StrictWriter.WriterT m) = StrictWriter.WriterT $ absolve (x, mempty) m
    condemn (StrictWriter.WriterT m) = StrictWriter.WriterT $ condemn m
    retcon f (StrictWriter.WriterT m) = StrictWriter.WriterT $ retcon f m
    chronicle = lift . chronicle

instance (Monoid w, MonadChronicle c m) => MonadChronicle c (LazyRWS.RWST r w s m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (LazyRWS.RWST m) = LazyRWS.RWST $ \r s ->
        either (\c -> (Left c, s, mempty)) (\(a, s', w) -> (Right a, s', w)) `liftM` memento (m r s)
    absolve x (LazyRWS.RWST m) = LazyRWS.RWST $ \r s -> absolve (x, s, mempty) $ m r s
    condemn (LazyRWS.RWST m) = LazyRWS.RWST $ \r s -> condemn $ m r s
    retcon f (LazyRWS.RWST m) = LazyRWS.RWST $ \r s -> retcon f $ m r s
    chronicle = lift . chronicle

instance (Monoid w, MonadChronicle c m) => MonadChronicle c (StrictRWS.RWST r w s m) where
    dictate = lift . dictate
    confess = lift . confess
    memento (StrictRWS.RWST m) = StrictRWS.RWST $ \r s ->
        either (\c -> (Left c, s, mempty)) (\(a, s', w) -> (Right a, s', w)) `liftM` memento (m r s)
    absolve x (StrictRWS.RWST m) = StrictRWS.RWST $ \r s -> absolve (x, s, mempty) $ m r s
    condemn (StrictRWS.RWST m) = StrictRWS.RWST $ \r s -> condemn $ m r s
    retcon f (StrictRWS.RWST m) = StrictRWS.RWST $ \r s -> retcon f $ m r s
    chronicle = lift . chronicle



