{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Chronicle
--
-- Hybrid error/writer monad class that allows both accumulating outputs and 
-- aborting computation with a final output.
--
-- The expected use case is for computations with a notion of fatal vs. 
-- non-fatal errors.

-----------------------------------------------------------------------------
module Control.Monad.Trans.Chronicle ( 
                                     -- * The Chronicle monad
                                       Chronicle, chronicle, runChronicle
                                     -- * The ChronicleT monad transformer
                                     , ChronicleT(..)
                                     -- * Chronicle operations
                                     , dictate, disclose, confess
                                     , memento, absolve, condemn
                                     , retcon
                                     ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Default.Class
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Functor.Identity
import Data.Semigroup

import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class
import Prelude
import Data.These

-- --------------------------------------------------------------------------
-- | A chronicle monad parameterized by the output type @c@.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with 'mappend'.
type Chronicle c = ChronicleT c Identity

chronicle :: These c a -> Chronicle c a
chronicle = ChronicleT . Identity

runChronicle :: Chronicle c a -> These c a
runChronicle = runIdentity . runChronicleT

-- --------------------------------------------------------------------------
-- | The `ChronicleT` monad transformer.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with 'mappend'.
newtype ChronicleT c m a = ChronicleT { runChronicleT :: m (These c a) }

instance (Functor m) => Functor (ChronicleT c m) where
    fmap f (ChronicleT c) =  ChronicleT (fmap f <$> c)

instance (Semigroup c, Apply m) => Apply (ChronicleT c m) where
    ChronicleT f <.> ChronicleT x = ChronicleT ((<.>) <$> f <.> x)

instance (Semigroup c, Applicative m) => Applicative (ChronicleT c m) where
    pure = ChronicleT . pure . pure
    ChronicleT f <*> ChronicleT x = ChronicleT (liftA2 (<*>) f x)

instance (Semigroup c, Apply m, Monad m) => Bind (ChronicleT c m) where
    (>>-) = (>>=)

instance (Semigroup c, Monad m) => Monad (ChronicleT c m) where
    return = ChronicleT . return . return
    m >>= k = ChronicleT $ 
        do cx <- runChronicleT m
           case cx of 
               This  a   -> return (This a)
               That    x -> runChronicleT (k x)
               These a x -> do cy <- runChronicleT (k x)
                               return $ case cy of
                                            This  b   -> This (a <> b)
                                            That    y -> These a y
                                            These b y -> These (a <> b) y

instance (Semigroup c) => MonadTrans (ChronicleT c) where
    lift m = ChronicleT (That `liftM` m)

instance (Semigroup c, MonadIO m) => MonadIO (ChronicleT c m) where
    liftIO = lift . liftIO


instance (Semigroup c, Monoid c, Applicative m, Monad m) => Alternative (ChronicleT c m) where
    empty = mzero
    (<|>) = mplus

instance (Semigroup c, Monoid c, Monad m) => MonadPlus (ChronicleT c m) where
    mzero = confess mempty
    mplus x y = do x' <- memento x
                   case x' of
                       Left  _ -> y
                       Right r -> return r


instance (Semigroup c, MonadError e m) => MonadError e (ChronicleT c m) where
    throwError = lift . throwError
    catchError (ChronicleT m) c = ChronicleT $ catchError m (runChronicleT . c)


instance (Semigroup c, MonadReader r m) => MonadReader r (ChronicleT c m) where
    ask = lift ask
    local f (ChronicleT m) = ChronicleT $ local f m
    reader = lift . reader

instance (Semigroup c, MonadRWS r w s m) => MonadRWS r w s (ChronicleT c m) where

instance (Semigroup c, MonadState s m) => MonadState s (ChronicleT c m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Semigroup c, MonadWriter w m) => MonadWriter w (ChronicleT c m) where
    tell = lift . tell
    listen (ChronicleT m) = ChronicleT $ do
        (m', w) <- listen m
        return $ case m' of
                     This  c   -> This c
                     That    x -> That (x, w)
                     These c x -> These c (x, w)
    pass (ChronicleT m) = ChronicleT $ do
        pass $ these (\c -> (This c, id)) 
                     (\(x, f) -> (That x, f)) 
                     (\c (x, f) -> (These c x, f)) `liftM` m
    writer = lift . writer

-- this is basically copied from the instance for Either in transformers
-- need to test this to make sure it's actually sensible...?
instance (Semigroup c, MonadFix m) => MonadFix (ChronicleT c m) where
    mfix f = ChronicleT (mfix (runChronicleT . f . these (const bomb) id (flip const)))
      where bomb = error "mfix (ChronicleT): inner compuation returned This value"


-- | @'dictate' c@ is an action that records the output @c@.
--   
--   Equivalent to 'tell' for the 'Writer' monad.
dictate :: (Semigroup c, Monad m) => c -> ChronicleT c m ()
dictate c = ChronicleT $ return (These c ())

-- | @'disclose' c@ is an action that records the output @c@ and returns a
--   @'Default'@ value.
--
--   This is a convenience function for reporting non-fatal errors in one
--   branch a @case@, or similar scenarios when there is no meaningful 
--   result but a placeholder of sorts is needed in order to continue.
disclose :: (Default a, Semigroup c, Monad m) => c -> ChronicleT c m a
disclose c = dictate c >> return def

-- | @'confess' c@ is an action that ends with a final output @c@.
--   
--   Equivalent to 'throwError' for the 'Error' monad.
confess :: (Semigroup c, Monad m) => c -> ChronicleT c m a
confess c = ChronicleT $ return (This c)

-- | @'memento' m@ is an action that executes the action @m@, returning either
--   its record if it ended with 'confess', or its final value otherwise, with
--   any record added to the current record.
--
--   Similar to 'catchError' in the 'Error' monad, but with a notion of 
--   non-fatal errors (which are accumulated) vs. fatal errors (which are caught
--   without accumulating).
memento :: (Semigroup c, Monad m) => ChronicleT c m a -> ChronicleT c m (Either c a)
memento m = ChronicleT $ 
    do cx <- runChronicleT m
       return $ case cx of
                    This  a   -> That (Left a)
                    That    x -> That (Right x)
                    These a x -> These a (Right x)

-- | @'absolve' x m@ is an action that executes the action @m@ and discards any
--   record it had. The default value @x@ will be used if @m@ ended via 
--   'confess'.
absolve :: (Semigroup c, Monad m) => a -> ChronicleT c m a -> ChronicleT c m a
absolve x m = ChronicleT $ 
    do cy <- runChronicleT m
       return $ case cy of
                    This  _   -> That x
                    That    y -> That y
                    These _ y -> That y


-- | @'condemn' m@ is an action that executes the action @m@ and keeps its value
--   only if it had no record. Otherwise, the value (if any) will be discarded
--   and only the record kept.
--
--   This can be seen as converting non-fatal errors into fatal ones.
condemn :: (Semigroup c, Monad m) => ChronicleT c m a -> ChronicleT c m a
condemn (ChronicleT m) = ChronicleT $ do 
    m' <- m
    return $ case m' of
        This  x   -> This x
        That    y -> That y
        These x _ -> This x


-- | @'retcon' f m@ is an action that executes the action @m@ and applies the
--   function @f@ to its output, leaving the return value unchanged.
--   
--   Equivalent to 'censor' for the 'Writer' monad.
retcon :: (Semigroup c, Monad m) => (c -> c) -> ChronicleT c m a -> ChronicleT c m a
retcon f m = ChronicleT $ mapThis f `liftM` runChronicleT m

