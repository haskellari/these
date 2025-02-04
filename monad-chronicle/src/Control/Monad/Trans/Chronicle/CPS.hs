{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Chronicle
--
-- Hybrid error/writer monad class that allows both accumulating outputs and
-- aborting computation with a final output.
--
-- The expected use case is for computations with a notion of fatal vs.
-- non-fatal errors.

-----------------------------------------------------------------------------
module Control.Monad.Trans.Chronicle.CPS (
    -- * The Chronicle monad
    Chronicle, chronicle, runChronicle,
    -- * The ChronicleT monad transformer
    ChronicleT(..),
    -- * Chronicle operations
    dictate, disclose, confess,
    memento, absolve, condemn,
    retcon,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Default.Class
import Data.Functor.Identity
import Data.Semigroup

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class
import Data.These
import Data.These.Combinators     (mapHere)
import Prelude

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Apply (Apply (..))
import Data.Functor.Bind  (Bind (..))
#endif

-- --------------------------------------------------------------------------
-- | A chronicle monad parameterized by the output type @c@.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with '<>'.
type Chronicle c = ChronicleT c Identity

chronicle :: Semigroup c => Monad m => These c a -> ChronicleT c m a
chronicle (This c')    = ChronicleT $ \c -> let ct = c <> c' in ct `seq` pure (This ct)
chronicle (That     a) = ChronicleT $ \c -> pure (These c a)
chronicle (These c' a) = ChronicleT $ \c -> let ct = c <> c' in ct `seq` pure (These ct a)
{-# INLINE chronicle #-}

runChronicle :: Monoid c => Chronicle c a -> These c a
runChronicle = runIdentity . flip runChronicleT mempty
{-# INLINE runChronicle #-}

-- --------------------------------------------------------------------------
-- | The `ChronicleT` monad transformer.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with '<>'.
newtype ChronicleT c m a = ChronicleT { runChronicleT :: c -> m (These c a) }

instance (Functor m) => Functor (ChronicleT c m) where
    fmap f m = ChronicleT $ \c -> g <$> runChronicleT m c
      where
        g (This c)    = This c
        g (That    a) = That (f a)
        g (These c a) = These c (f a)
    {-# INLINE fmap #-}

#ifdef MIN_VERSION_semigroupoids
instance (Semigroup c, Monad m) => Apply (ChronicleT c m) where
    (<.>) = (<*>)

instance (Semigroup c, Monad m) => Bind (ChronicleT c m) where
    (>>-) = (>>=)
#endif

instance (Semigroup c, Monad m) => Applicative (ChronicleT c m) where
    pure a = ChronicleT $ \c -> pure (These c a)
    {-# INLINE pure #-}

    ChronicleT f <*> ChronicleT x = ChronicleT $ \c -> do
      t <- f c
      case t of
        This c' -> do
          t' <- x c'
          case t' of
            This  c''   -> pure (This c'')
            That      _ -> pure (This c')
            These c'' _ -> pure (This c'')
        That f' -> do
          t' <- x c
          case t' of
            This  c''     -> pure (This c'')
            That      x'' -> pure (That (f' x''))
            These c'' x'' -> pure (These c'' (f' x''))
        These c' f' -> do
          t' <- x c'
          case t' of
            This  c''     -> pure (This c'')
            That      x'' -> pure (These c' (f' x''))
            These c'' x'' -> pure (These c'' (f' x''))
    {-# INLINE (<*>) #-}

instance (Semigroup c, Monad m) => Monad (ChronicleT c m) where
    return a = ChronicleT $ \c -> return (These c a)
    {-# INLINE return #-}

    m >>= k = ChronicleT $ \c -> do
      t <- runChronicleT m c
      case t of
        This c'    -> return (This c')
        That    a  -> runChronicleT (k a) c
        These c' a -> runChronicleT (k a) c'
    {-# INLINE (>>=) #-}

instance (Semigroup c) => MonadTrans (ChronicleT c) where
    lift m = ChronicleT $ \c -> (These c `liftM` m)
    {-# INLINE lift #-}

instance (Semigroup c, MonadIO m) => MonadIO (ChronicleT c m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Semigroup c, Monoid c, Applicative m, Monad m) => Alternative (ChronicleT c m) where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance (Semigroup c, Monoid c, Monad m) => MonadPlus (ChronicleT c m) where
    mzero = confess mempty
    {-# INLINE mzero #-}
    mplus x y = do x' <- memento x
                   case x' of
                       Left  _ -> y
                       Right r -> return r
    {-# INLINE mplus #-}

instance (Semigroup c, MonadError e m) => MonadError e (ChronicleT c m) where
    throwError = lift . throwError
    {-# INLINE throwError #-}
    catchError (ChronicleT m) c = ChronicleT $ \c' -> catchError (m c') (flip runChronicleT c' . c)
    {-# INLINE catchError #-}

instance (Semigroup c, MonadReader r m) => MonadReader r (ChronicleT c m) where
    ask = lift ask
    {-# INLINE ask #-}
    local f (ChronicleT m) = ChronicleT $ \c -> local f (m c)
    {-# INLINE local #-}
    reader = lift . reader
    {-# INLINE reader #-}

instance (Semigroup c, MonadRWS r w s m) => MonadRWS r w s (ChronicleT c m) where

instance (Semigroup c, MonadState s m) => MonadState s (ChronicleT c m) where
    get = lift get
    {-# INLINE get #-}
    put = lift . put
    {-# INLINE put #-}
    state = lift . state
    {-# INLINE state #-}

instance (Semigroup c, MonadWriter w m) => MonadWriter w (ChronicleT c m) where
    tell = lift . tell
    {-# INLINE tell #-}
    listen (ChronicleT m) = ChronicleT $ \c' -> do
        (m', w) <- listen (m c')
        return $ case m' of
                     This  c   -> This c
                     That    x -> That (x, w)
                     These c x -> These c (x, w)
    {-# INLINE listen #-}
    pass (ChronicleT m) = ChronicleT $ \c' -> do
        pass $ these (\c -> (This c, id))
                     (\(x, f) -> (That x, f))
                     (\c (x, f) -> (These c x, f)) `liftM` (m c')
    {-# INLINE pass #-}
    writer = lift . writer
    {-# INLINE writer #-}

-- this is basically copied from the instance for Either in transformers
-- need to test this to make sure it's actually sensible...?
instance (Semigroup c, MonadFix m) => MonadFix (ChronicleT c m) where
    mfix f = ChronicleT $ \c -> (mfix (flip runChronicleT c . f . these (const bomb) id (flip const)))
      where bomb = error "mfix (ChronicleT): inner compuation returned This value"
    {-# INLINE mfix #-}

-- | @'dictate' c@ is an action that records the output @c@.
--
--   Equivalent to 'tell' for the 'Writer' monad.
dictate :: (Semigroup c, Monad m) => c -> ChronicleT c m ()
dictate c' = ChronicleT $ \c -> let ct = c <> c' in ct `seq` pure (These ct ())
{-# INLINE dictate #-}

-- | @'disclose' c@ is an action that records the output @c@ and returns a
--   @'Default'@ value.
--
--   This is a convenience function for reporting non-fatal errors in one
--   branch a @case@, or similar scenarios when there is no meaningful
--   result but a placeholder of sorts is needed in order to continue.
disclose :: (Default a, Semigroup c, Monad m) => c -> ChronicleT c m a
disclose c = dictate c >> return def
{-# INLINE disclose #-}

-- | @'confess' c@ is an action that ends with a final output @c@.
--
--   Equivalent to 'throwError' for the 'Error' monad.
confess :: (Semigroup c, Monad m) => c -> ChronicleT c m a
confess c' = ChronicleT $ \c -> let ct = c <> c' in ct `seq` pure (This ct)
{-# INLINE confess #-}

-- | @'memento' m@ is an action that executes the action @m@, returning either
--   its record if it ended with 'confess', or its final value otherwise, with
--   any record added to the current record.
--
--   Similar to 'catchError' in the 'Error' monad, but with a notion of
--   non-fatal errors (which are accumulated) vs. fatal errors (which are caught
--   without accumulating).
memento :: (Semigroup c, Monad m) => ChronicleT c m a -> ChronicleT c m (Either c a)
memento m = ChronicleT $ \c ->
    do cx <- runChronicleT m c
       return $ case cx of
                    This  a   -> That (Left a)
                    That    x -> That (Right x)
                    These a x -> These a (Right x)
{-# INLINE memento #-}

-- | @'absolve' x m@ is an action that executes the action @m@ and discards any
--   record it had. The default value @x@ will be used if @m@ ended via
--   'confess'.
absolve :: (Semigroup c, Monad m) => a -> ChronicleT c m a -> ChronicleT c m a
absolve x m = ChronicleT $ \c ->
    do cy <- runChronicleT m c
       return $ case cy of
                    This  _   -> That x
                    That    y -> That y
                    These _ y -> That y
{-# INLINE absolve #-}


-- | @'condemn' m@ is an action that executes the action @m@ and keeps its value
--   only if it had no record. Otherwise, the value (if any) will be discarded
--   and only the record kept.
--
--   This can be seen as converting non-fatal errors into fatal ones.
condemn :: (Semigroup c, Monad m) => ChronicleT c m a -> ChronicleT c m a
condemn (ChronicleT m) = ChronicleT $ \c -> do
    m' <- m c
    return $ case m' of
        This  x   -> This x
        That    y -> That y
        These x _ -> This x
{-# INLINE condemn #-}


-- | @'retcon' f m@ is an action that executes the action @m@ and applies the
--   function @f@ to its output, leaving the return value unchanged.
--
--   Equivalent to 'censor' for the 'Writer' monad.
retcon :: (Semigroup c, Monad m) => (c -> c) -> ChronicleT c m a -> ChronicleT c m a
retcon f m = ChronicleT $ \c -> mapHere f `liftM` runChronicleT m c
{-# INLINE retcon #-}
