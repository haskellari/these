{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Trans.Chronicle
--
-- The 'ChronicleT' monad, a hybrid error/writer monad that allows
-- both accumulating outputs and aborting computation with a final
-- output.
-----------------------------------------------------------------------------
module Control.Monad.Trans.Chronicle ( 
                                     -- * The Chronicle monad
                                       Chronicle, chronicle, runChronicle
                                     -- * The ChronicleT monad transformer
                                     , ChronicleT(..)
                                     -- * Chronicle operations
                                     , dictate, confess
                                     , memento, absolve, condemn
                                     , retcon
                                     ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Functor.Identity
import Data.Monoid

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

instance (Monoid c, Apply m) => Apply (ChronicleT c m) where
    ChronicleT f <.> ChronicleT x = ChronicleT ((<.>) <$> f <.> x)

instance (Monoid c, Applicative m) => Applicative (ChronicleT c m) where
    pure = ChronicleT . pure . pure
    ChronicleT f <*> ChronicleT x = ChronicleT (liftA2 (<*>) f x)

instance (Monoid c, Apply m, Monad m) => Bind (ChronicleT c m) where
    (>>-) = (>>=)

instance (Monoid c, Monad m) => Monad (ChronicleT c m) where
    return = ChronicleT . return . return
    m >>= k = ChronicleT $ 
        do cx <- runChronicleT m
           case cx of 
               This  a   -> return (This a)
               That    x -> runChronicleT (k x)
               These a x -> do cy <- runChronicleT (k x)
                               return $ case cy of
                                            This  b   -> This (mappend a b)
                                            That    y -> These a y
                                            These b y -> These (mappend a b) y

instance (Monoid c) => MonadTrans (ChronicleT c) where
    lift m = ChronicleT (That `liftM` m)

instance (Monoid c, MonadIO m) => MonadIO (ChronicleT c m) where
    liftIO = lift . liftIO


instance (Monoid c, Applicative m, Monad m) => Alternative (ChronicleT c m) where
    empty = mzero
    (<|>) = mplus

instance (Monoid c, Monad m) => MonadPlus (ChronicleT c m) where
    mzero = confess mempty
    mplus x y = do x' <- memento x
                   case x' of
                       Left  _ -> y
                       Right r -> return r


instance (Monoid c, MonadError e m) => MonadError e (ChronicleT c m) where
    throwError = lift . throwError
    catchError (ChronicleT m) c = ChronicleT $ catchError m (runChronicleT . c)


instance (Monoid c, MonadReader r m) => MonadReader r (ChronicleT c m) where
    ask = lift ask
    local f (ChronicleT m) = ChronicleT $ local f m
    reader = lift . reader

instance (Monoid c, MonadRWS r w s m) => MonadRWS r w s (ChronicleT c m) where

instance (Monoid c, MonadState s m) => MonadState s (ChronicleT c m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid c, MonadWriter w m) => MonadWriter w (ChronicleT c m) where
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



-- | @'dictate' c@ is an action that records the output @c@.
--   
--   Equivalent to 'tell' for the 'Writer' monad.
dictate :: (Monoid c, Monad m) => c -> ChronicleT c m ()
dictate c = ChronicleT $ return (These c ())

-- | @'confess' c@ is an action that ends with a final output @c@.
--   
--   Equivalent to 'throwError' for the 'Error' monad.
confess :: (Monoid c, Monad m) => c -> ChronicleT c m a
confess c = ChronicleT $ return (This c)

-- | @'memento' m@ is an action that executes the action @m@, returning either
--   its record if it ended with 'confess', or its final value otherwise, with
--   any record added to the current record.
--
--   Similar to 'catchError' in the 'Error' monad, but with a notion of 
--   non-fatal errors (which are accumulated) vs. fatal errors (which are caught
--   without accumulating).
memento :: (Monoid c, Monad m) => ChronicleT c m a -> ChronicleT c m (Either c a)
memento m = ChronicleT $ 
    do cx <- runChronicleT m
       return $ case cx of
                    This  a   -> That (Left a)
                    That    x -> That (Right x)
                    These a x -> These a (Right x)

-- | @'absolve' x m@ is an action that executes the action @m@ and discards any
--   record it had. The default value @x@ will be used if @m@ ended via 
--   'confess'.
absolve :: (Monoid c, Monad m) => a -> ChronicleT c m a -> ChronicleT c m a
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
condemn :: (Monoid c, Monad m) => ChronicleT c m a -> ChronicleT c m a
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
retcon :: (Monoid c, Monad m) => (c -> c) -> ChronicleT c m a -> ChronicleT c m a
retcon f m = ChronicleT $ mapThis f `liftM` runChronicleT m

